{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Chkfs where

--------------------------------------------------------------------------------

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           System.Exit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners

--------------------------------------------------------------------------------

_BSIZE :: Word32
_BSIZE = 1024


_FSMAGIC :: Word32
_FSMAGIC = 0x10203040

--------------------------------------------------------------------------------

parseImage :: String -> Word32 -> BL.ByteString -> Image
parseImage imgName imgSize = runGet (getImage imgName imgSize)

--------------------------------------------------------------------------------

data Image = Image
    { imgName :: !String
    , imgSize :: {-# UNPACK #-} !Word32
    , imgSb   :: !SuperBlock
    }
    deriving (Show)


getImage :: String -> Word32 -> Get Image
getImage imgName imgSize = do
    skip 1024 -- skip boot block
    imgSb <- getSuperBlock
    pure Image{..}

--------------------------------------------------------------------------------

data SuperBlock = SuperBlock
    { sbMagic      :: {-# UNPACK #-} !Word32
    , sbSize       :: {-# UNPACK #-} !Word32
    , sbNblocks    :: {-# UNPACK #-} !Word32
    , sbNinodes    :: {-# UNPACK #-} !Word32
    , sbNlog       :: {-# UNPACK #-} !Word32
    , sbLogstart   :: {-# UNPACK #-} !Word32
    , sbInodestart :: {-# UNPACK #-} !Word32
    , sbBmapstart  :: {-# UNPACK #-} !Word32
    }
    deriving (Show)


getSuperBlock :: Get SuperBlock
getSuperBlock =
    SuperBlock
        <$> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host
        <*> getWord32host

--------------------------------------------------------------------------------

runCheck :: TestTree -> IO ()
runCheck testTree = do
    installSignalHandlers

    sequence (tryIngredients defaultIngredients mempty testTree) >>= \case
        Just True -> do
            exitSuccess

        _ -> do
            exitFailure

--------------------------------------------------------------------------------

tests :: Image -> TestTree
tests img@Image{..} =
    testGroup imgName
        [ testsSb img
        ]


testsSb :: Image -> TestTree
testsSb Image{ imgSb = SuperBlock{..}, .. } =
    testGroup "super block"
        [ testCase "sbMagic should be _FSMAGIC" $
            sbMagic @?= _FSMAGIC

        , testCase "sbSize should be imgSize/_BSIZE" $
            sbSize @?= (imgSize `div` _BSIZE)
        ]
