{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Chkfs where

--------------------------------------------------------------------------------

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Foldable
import           Data.Int
import qualified Data.Vector.Storable.Sized as VS
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Generic
import           GHC.Generics
import           GHC.TypeLits
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.Hspec.Core.Spec

--------------------------------------------------------------------------------

_BSIZE :: Word32
_BSIZE = 1024


_FSMAGIC :: Word32
_FSMAGIC = 0x10203040


_IPB :: Word32
_IPB = 16

--------------------------------------------------------------------------------

type Image = Ptr ()


index :: Integral a => Image -> a -> Ptr ()
index img n = img `plusPtr` (fromIntegral n * fromIntegral _BSIZE)

--------------------------------------------------------------------------------

data Superblock = Superblock
    { sbMagic      :: {-# UNPACK #-} !Word32
    , sbSize       :: {-# UNPACK #-} !Word32
    , sbNblocks    :: {-# UNPACK #-} !Word32
    , sbNinodes    :: {-# UNPACK #-} !Word32
    , sbNlog       :: {-# UNPACK #-} !Word32
    , sbLogstart   :: {-# UNPACK #-} !Word32
    , sbInodestart :: {-# UNPACK #-} !Word32
    , sbBmapstart  :: {-# UNPACK #-} !Word32
    }
    deriving (Show, Generic, GStorable)


getSuperblock :: Image -> IO Superblock
getSuperblock img = peek (castPtr (img `index` 1) :: Ptr Superblock)

--------------------------------------------------------------------------------

isBlockUsed :: Image -> Word32 -> IO Bool
isBlockUsed img bnum = do
    Superblock{..} <- getSuperblock img

    let !ptr = castPtr (img `index` sbBmapstart) :: Ptr Word8

    w <- peekElemOff ptr (fromIntegral bnum `div` 8)
    pure $! testBit w (fromIntegral bnum `mod` 8)

--------------------------------------------------------------------------------

type NDIRECT = 12


_T_DIR, _T_FILE, _T_DEV :: Int16
_T_DIR  = 1
_T_FILE = 2
_T_DEV  = 3


data Dinode = Dinode
    { diType  :: {-# UNPACK #-} !Int16
    , diMajor :: {-# UNPACK #-} !Int16
    , diMinor :: {-# UNPACK #-} !Int16
    , diNlink :: {-# UNPACK #-} !Int16
    , diSize  :: {-# UNPACK #-} !Word32
    , diAddrs :: !(VS.Vector (NDIRECT + 1) Word32)
    }
    deriving (Show, Generic, GStorable)


isNullDinode :: Dinode -> Bool
isNullDinode Dinode{..} =
    and
        [ diType == 0
        , diMajor == 0
        , diMinor == 0
        , diSize == 0
        , VS.all (== 0) diAddrs
        ]


getNthDinode :: Image -> Word32 -> IO Dinode
getNthDinode img n = do
    Superblock{..} <- getSuperblock img
    let !ptr = castPtr (img `index` sbInodestart) :: Ptr Dinode
    peekElemOff ptr (fromIntegral n)


foreachAddrs
    :: MonadIO m
    => Image
    -> VS.Vector (NDIRECT + 1) Word32
    -> (Word32 -> m ())
    -> m ()
foreachAddrs img addrs f = do
    VS.forM_ (VS.init addrs) \addr ->
        when (addr /= 0) (f addr)

    -- indirect block
    let ptr = castPtr (img `index` VS.last addrs) :: Ptr Word32
    for_ [0 .. fromIntegral _BSIZE `div` 4 - 1] \i -> do
        addr <- liftIO $ peekByteOff ptr i
        when (addr /= 0) (f addr)

--------------------------------------------------------------------------------

type DIRSIZ = 14


data Dirent = Dirent
    { deInum :: {-# UNPACK #-} !Word16
    , deName :: !(VS.Vector DIRSIZ Word8)
    }
    deriving (Generic, GStorable)


instance Show Dirent where
    show Dirent{..} =
        concat
            [ "Dirent { deInum = "
            , show deInum
            , ", deName = "
            , showDeName deName
            , " }"
            ]


showDeName :: VS.Vector DIRSIZ Word8 -> String
showDeName = map (toEnum . fromIntegral) . filter (/= 0) . VS.toList


isNullDirent :: Dirent -> Bool
isNullDirent Dirent{..} = (deInum == 0) && VS.all (== 0) deName

--------------------------------------------------------------------------------

-- Orphan instance
instance MonadIO (SpecM a) where
    liftIO = runIO

--------------------------------------------------------------------------------

doCheck :: Spec -> IO ()
doCheck spec = runSpec spec defaultConfig >>= evaluateSummary

--------------------------------------------------------------------------------

superblockSpec :: Image -> Spec
superblockSpec img =
    describe "superblock" do
        Superblock{..} <- runIO $ getSuperblock img

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


inodesSpec :: Image -> Spec
inodesSpec img =
    describe "inodes" do
        Superblock{..} <- runIO $ getSuperblock img

        for_ [0 .. sbNinodes - 1] \inum -> do
            dind <- runIO $ getNthDinode img inum
            nthInodeSpec img inum dind


nthInodeSpec :: Image -> Word32 -> Dinode -> Spec
nthInodeSpec img inum dind@Dinode{..} =
    describe ("inode " ++ show inum) do
        unless (isNullDinode dind) do
            specify "type should be T_DIR, T_FILE or T_DEV" do
                diType `shouldSatisfy` (`elem` [_T_DIR, _T_FILE, _T_DEV])

            when (diType == _T_DEV) do
                specify "(major, minor) should not be (0, 0)" do
                    (diMajor, diMinor) `shouldNotBe` (0, 0)

            VS.forM_ diAddrs \addr -> do
                when (addr /= 0) do
                    specify ("addr " ++ show addr ++ " should refer used block") do
                        isBlockUsed img addr `shouldReturn` True

            when (diType == _T_DIR) do
                foreachAddrs img diAddrs \addr -> do
                    let ptr = castPtr (img `index` addr) :: Ptr Dirent

                    for_ [0 .. fromIntegral _BSIZE `div` 16 - 1] \n -> do
                        de@Dirent{..} <- runIO $ peekElemOff ptr n

                        unless (isNullDirent de) do
                            dind' <- runIO $ getNthDinode img (fromIntegral deInum)
                            specify ("dirent named " ++ showDeName deName ++ " should refer used inode") do
                                dind' `shouldNotSatisfy` isNullDinode
