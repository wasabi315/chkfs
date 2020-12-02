{-# LANGUAGE LambdaCase #-}

module Chkfs
    ( doCheck
    , SuperBlock (..)
    , getSuperBlock
    , _FSMAGIC
    ) where

--------------------------------------------------------------------------------

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           System.Exit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners

--------------------------------------------------------------------------------

doCheck :: BL.ByteString -> IO ()
doCheck img = do
    let sb = runGet (skip 1024 >> getSuperBlock) img
    runCheck (testsSB sb)


runCheck :: TestTree -> IO ()
runCheck testTree = do
    installSignalHandlers
    sequence (tryIngredients defaultIngredients mempty testTree) >>= \case
        Just True -> do
            exitSuccess

        _ -> do
            exitFailure


testsSB :: SuperBlock -> TestTree
testsSB sb = testGroup "super block"
    [ testCase "sbMagic must be _FSMAGIC" $
        sbMagic sb @?= _FSMAGIC
    ]

--------------------------------------------------------------------------------

_FSMAGIC :: Word32
_FSMAGIC = 0x10203040

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
