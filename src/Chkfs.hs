{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Chkfs where

--------------------------------------------------------------------------------

import           Data.ByteString.Internal   (w2c)
import           Data.Int
import qualified Data.Vector.Storable.Sized as VS
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Generic
import           GHC.Generics
import           GHC.TypeLits
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


type NDIRECT = 12


type DIRSIZ = 14

--------------------------------------------------------------------------------

type Image = Ptr ()


(!) :: Integral a => Image -> a -> Ptr ()
img ! n = img `plusPtr` (fromIntegral n * fromIntegral _BSIZE)

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


data Dinode = Dinode
    { diInum  :: {-# UNPACK #-} !Word32
    , diType  :: {-# UNPACK #-} !Int16
    , diMajor :: {-# UNPACK #-} !Int16
    , diMinor :: {-# UNPACK #-} !Int16
    , diNlink :: {-# UNPACK #-} !Int16
    , diSize  :: {-# UNPACK #-} !Word32
    , diAddrs :: !(VS.Vector (NDIRECT + 1) Word32)
    }
    deriving (Show, Generic, GStorable)


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

createTests :: String -> Int -> Image -> TestTree
createTests imgName imgSize img = testCaseSteps imgName \step -> do

    step "Checking super block..."
    Superblock{..} <- peek (castPtr (img ! 1) :: Ptr Superblock)

    do
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
