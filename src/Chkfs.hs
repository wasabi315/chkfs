{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Chkfs where

--------------------------------------------------------------------------------

import           Control.Monad
import           Data.Bits
import           Data.ByteString.Internal   (w2c)
import           Data.Char
import           Data.Foldable
import           Data.Int
import qualified Data.Vector.Storable.Sized as VS
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Generic
import           GHC.Generics
import           GHC.TypeLits
import           Numeric
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners

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

newtype Bitmap = Bitmap Integer


instance Show Bitmap where
    show (Bitmap bm) = showIntAtBase 2 intToDigit bm ""


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
    assertBool ("invalid inode number: " ++ show n) (n < sbNinodes)

    let !ptr = castPtr (img `index` sbInodestart) :: Ptr Dinode
    peekElemOff ptr (fromIntegral n)


testNthDinode :: Image -> Word32 -> Dinode -> IO ()
testNthDinode img inum dind@Dinode{..} =
    unless (isNullDinode dind) do
        assertBool ("inode" ++ show inum ++ ": invalid file type") $
            diType `elem` [_T_DIR, _T_FILE, _T_DEV]

        when (diType == _T_DEV) $
            assertBool ("inode" ++ show inum ++ ": invalid major, minor") $
                (diMajor /= 0) || (diMinor /= 0)

        VS.forM_ diAddrs \addr -> do
            assertBool ("inode" ++ show inum ++ ": invalid addr referencing unused block") =<<
                isBlockUsed img addr

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
            , map w2c (VS.toList deName)
            , " }"
            ]


isNullDirent :: Dirent -> Bool
isNullDirent Dirent{..} = (deInum == 0) && VS.all (== 0) deName

--------------------------------------------------------------------------------

runTest :: TestTree -> IO Bool
runTest testTree = do
    installSignalHandlers

    sequence (tryIngredients defaultIngredients mempty testTree) >>= \case
        Just True -> do
            pure True

        _ -> do
            pure False

--------------------------------------------------------------------------------

createTests :: String -> Image -> TestTree
createTests imgName img = testCaseSteps imgName \step -> do

    step "Checking superblock ..."
    sb@Superblock{..} <- getSuperblock img
    testSuperblock sb

    step "Checking inodes ..."
    for_ [0 .. sbNinodes - 1] \inum -> do
        dind <- getNthDinode img inum
        testNthDinode img inum dind


testSuperblock :: Superblock -> Assertion
testSuperblock Superblock{..} = do
    let nb = 1
        ns = 1
        nl = sbNlog
        ni = sbNinodes `div` _IPB + 1
        nm = sbSize `div` (_BSIZE * 8) + 1
        nd = sbSize - (nb + ns + nl + ni + nm)

    assertBool "sbMagic was not FSMAGIC" $
        sbMagic == _FSMAGIC

    assertBool "sbSize was not total count of blocks" $
        sbSize == (nb + ns + nl + ni + nm + nd)

    assertBool "sbNblock was not consistent" $
        sbNblocks == nd

    assertBool "sbNlog was not consistent" $
        sbNlog == nl

    assertBool "sbLogstart was not consistent" $
        sbLogstart == (nb + ns)

    assertBool "sbInodestart was not consistent" $
        sbInodestart == (nb + ns + nl)

    assertBool "sbBmapstart was not consistent" $
        sbBmapstart == (nb + ns + nl + ni)
