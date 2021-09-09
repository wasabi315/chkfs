{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Chkfs where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.Foldable
import Data.Vector.Storable.Sized qualified as VS
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import GHC.TypeLits
import System.IO.MMap
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec
import Text.Show qualified

--------------------------------------------------------------------------------

chkfs :: FilePath -> IO Bool
chkfs imgName = do
  mmapWithFilePtr imgName ReadOnly Nothing \(img, _) -> do
    sb <- getSuperblock img
    doCheck (superblockSpec sb) >>= \case
      False -> pure False
      True ->
        doCheck (inodesSpec img sb) >>= \case
          False -> pure False
          True -> pure True

--------------------------------------------------------------------------------

_BSIZE :: CUInt
_BSIZE = 1024

_FSMAGIC :: CUInt
_FSMAGIC = 0x10203040

_IPB :: CUInt
_IPB = 16

--------------------------------------------------------------------------------

type Image = Ptr ()

index :: Integral a => Image -> a -> Ptr ()
index img n = img `plusPtr` (fromIntegral n * fromIntegral _BSIZE)

--------------------------------------------------------------------------------

data Superblock = Superblock
  { sbMagic :: {-# UNPACK #-} !CUInt,
    sbSize :: {-# UNPACK #-} !CUInt,
    sbNblocks :: {-# UNPACK #-} !CUInt,
    sbNinodes :: {-# UNPACK #-} !CUInt,
    sbNlog :: {-# UNPACK #-} !CUInt,
    sbLogstart :: {-# UNPACK #-} !CUInt,
    sbInodestart :: {-# UNPACK #-} !CUInt,
    sbBmapstart :: {-# UNPACK #-} !CUInt
  }
  deriving (Show, Generic, GStorable)

getSuperblock :: Image -> IO Superblock
getSuperblock img = peek (castPtr (img `index` (1 :: Int)) :: Ptr Superblock)

--------------------------------------------------------------------------------

isBlockUsed :: Image -> Superblock -> CUInt -> IO Bool
isBlockUsed img Superblock {..} bnum = do
  let !ptr = castPtr (img `index` sbBmapstart) :: Ptr Word8
  w <- peekElemOff ptr (fromIntegral bnum `div` 8)
  pure $! testBit w (fromIntegral bnum `mod` 8)

--------------------------------------------------------------------------------

type NDIRECT = 12

_T_DIR, _T_FILE, _T_DEV :: CShort
_T_DIR = 1
_T_FILE = 2
_T_DEV = 3

_ROOTINO :: CUInt
_ROOTINO = 1

data Dinode = Dinode
  { diType :: {-# UNPACK #-} !CShort,
    diMajor :: {-# UNPACK #-} !CShort,
    diMinor :: {-# UNPACK #-} !CShort,
    diNlink :: {-# UNPACK #-} !CShort,
    diSize :: {-# UNPACK #-} !CUInt,
    diAddrs :: !(VS.Vector (NDIRECT + 1) CUInt)
  }
  deriving (Show, Generic, GStorable)

isNullInode :: Dinode -> Bool
isNullInode Dinode {..} =
  and
    [ diType == 0,
      diMajor == 0,
      diMinor == 0,
      diSize == 0,
      VS.all (== 0) diAddrs
    ]

getInode :: Image -> Superblock -> CUInt -> IO Dinode
getInode img Superblock {..} n = do
  let !ptr = castPtr (img `index` sbInodestart) :: Ptr Dinode
  peekElemOff ptr (fromIntegral n)

foreachAddrs ::
  MonadIO m =>
  Image ->
  VS.Vector (NDIRECT + 1) CUInt ->
  (CUInt -> m ()) ->
  m ()
foreachAddrs img addrs f = do
  VS.forM_ (VS.init addrs) \addr ->
    when (addr /= 0) (f addr)

  -- indirect block
  let ptr = castPtr (img `index` VS.last addrs) :: Ptr CUInt
  for_ [0 .. fromIntegral _BSIZE `div` 4 - 1] \i -> do
    addr <- liftIO $ peekByteOff ptr i
    when (addr /= 0) (f addr)

--------------------------------------------------------------------------------

type DIRSIZ = 14

data Dirent = Dirent
  { deInum :: {-# UNPACK #-} !CUShort,
    deName :: !(VS.Vector DIRSIZ CChar)
  }
  deriving (Generic, GStorable)

instance Show Dirent where
  showsPrec _ Dirent {..} =
    Text.Show.showString "Dirent{deInum="
      . Text.Show.shows deInum
      . Text.Show.showString ",deName="
      . Text.Show.shows (showDeName deName)
      . Text.Show.showChar '}'

showDeName :: VS.Vector DIRSIZ CChar -> String
showDeName = map (toEnum . fromIntegral) . filter (/= 0) . VS.toList

isNullDirent :: Dirent -> Bool
isNullDirent Dirent {..} = (deInum == 0) && VS.all (== 0) deName

--------------------------------------------------------------------------------

-- Orphan instance
instance MonadIO (SpecM a) where
  liftIO = runIO

--------------------------------------------------------------------------------

doCheck :: Spec -> IO Bool
doCheck spec = isSuccess <$> runSpec spec defaultConfig

--------------------------------------------------------------------------------

superblockSpec :: Superblock -> Spec
superblockSpec Superblock {..} =
  describe "superblock" do
    let nb = 1
        ns = 1
        nl = sbNlog
        ni = sbNinodes `div` _IPB + 1
        nm = sbSize `div` (_BSIZE * 8) + 1
        nd = sbSize - (nb + ns + nl + ni + nm)

    specify "sbMagic should be FSMAGIC" do
      sbMagic `shouldBe` _FSMAGIC

    specify "sbSize was not total count of blocks" do
      sbSize `shouldBe` (nb + ns + nl + ni + nm + nd)

    specify "sbNblock should be consistent" do
      sbNblocks `shouldBe` nd

    specify "sbNlog should be consistent" do
      sbNlog `shouldBe` nl

    specify "sbLogstart should be consistent" do
      sbLogstart `shouldBe` (nb + ns)

    specify "sbInodestart should be consistent" do
      sbInodestart `shouldBe` (nb + ns + nl)

    specify "sbBmapstart should be consistent" do
      sbBmapstart `shouldBe` (nb + ns + nl + ni)

inodesSpec :: Image -> Superblock -> Spec
inodesSpec img sb =
  describe "inode" do
    go _ROOTINO _ROOTINO
  where
    dot, dotdot :: VS.Vector DIRSIZ CChar
    dot = VS.fromTuple (46, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    dotdot = VS.fromTuple (46, 46, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    go :: CUInt -> CUInt -> Spec
    go pino ino = do
      ind <- runIO $ getInode img sb ino

      specify ("inode " ++ show ino ++ " should be used") do
        ind `shouldNotSatisfy` isNullInode

      specify "type should be T_DIR, T_FILE or T_DEV" do
        diType ind `shouldSatisfy` (`elem` [_T_DIR, _T_FILE, _T_DEV])

      when (diType ind == _T_DEV) do
        specify "(major, minor) should not be (0, 0)" do
          (diMajor ind, diMinor ind) `shouldNotBe` (0, 0)

      VS.forM_ (diAddrs ind) \addr -> do
        when (addr /= 0) do
          specify ("addr " ++ show addr ++ " should refer used block") do
            isBlockUsed img sb addr `shouldReturn` True

      when (diType ind == _T_DIR) do
        foreachAddrs img (diAddrs ind) \addr -> do
          let !ptr = castPtr (img `index` addr) :: Ptr Dirent

          for_ [0 .. fromIntegral _BSIZE `div` 16 - 1] \n -> do
            de@Dirent {..} <- runIO $ peekElemOff ptr n

            unless (isNullDirent de) do
              let !ino' = fromIntegral deInum :: CUInt

              when (deName == dot) do
                specify ". should refer current directory" do
                  ino' `shouldBe` ino

              when (deName == dotdot) do
                specify ".. should refer parent directory" do
                  ino' `shouldBe` pino

              when (ino' /= pino && ino' /= ino) do
                go ino ino'
