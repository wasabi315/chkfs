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
    [ testCase "magic must be _FSMAGIC" $
        magic sb @?= _FSMAGIC
    ]

--------------------------------------------------------------------------------

_FSMAGIC :: Word32
_FSMAGIC = 0x10203040

--------------------------------------------------------------------------------

data SuperBlock = SuperBlock
    { magic      :: {-# UNPACK #-} !Word32
    , size       :: {-# UNPACK #-} !Word32
    , nblocks    :: {-# UNPACK #-} !Word32
    , ninodes    :: {-# UNPACK #-} !Word32
    , nlog       :: {-# UNPACK #-} !Word32
    , logstart   :: {-# UNPACK #-} !Word32
    , inodestart :: {-# UNPACK #-} !Word32
    , bmapstart  :: {-# UNPACK #-} !Word32
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
