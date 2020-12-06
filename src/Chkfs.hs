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

import           Data.Bits
import           Data.ByteString.Internal   (w2c)
import           Data.Char
import           Data.Foldable
import           Data.Int
import           Data.Traversable
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


getBitmap :: Image -> Superblock -> IO Bitmap
getBitmap img Superblock{..} =
    let
        nBytes :: Int
        !nBytes = fromIntegral $ (sbSize - 1) `div` 8 + 1 -- ceil(size / 8)

        ptr :: Ptr Word8
        !ptr = castPtr (img `index` sbBmapstart)

        go :: Int -> Integer -> IO Bitmap
        go !n !bm
            | n >= nBytes = pure (Bitmap bm)
            | otherwise = do
                w <- peekElemOff ptr n
                go (n + 1) (bm .|. shiftL (fromIntegral w) (n * 8))
    in
        go 0 0

--------------------------------------------------------------------------------

type NDIRECT = 12


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


getDinodes :: Image -> Superblock -> IO [Dinode]
getDinodes img Superblock{..} =
    let
        ptr :: Ptr Dinode
        !ptr = castPtr (img `index` sbInodestart)
    in
        for [0 .. fromIntegral sbNinodes - 1] (peekElemOff ptr)

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

    step "Extracting superblock"
    sb@Superblock{..} <- getSuperblock img
    step (show sb)
    testSuperblock sb

    step "Extracting bitmap"
    bm <- getBitmap img sb
    step (show bm)

    step "Extracting dinodes"
    dinodes <- getDinodes img sb
    for_ dinodes (step . show)


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
