{-# LANGUAGE BlockArguments #-}

module Main where

--------------------------------------------------------------------------------

import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           System.Environment
import           System.IO

import           Chkfs

--------------------------------------------------------------------------------

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    when (null args) do
        hPutStr stderr $ "Usage: " ++ prog ++ " FILE"

    let imgName = head args
    img <- withBinaryFile imgName ReadMode $ \h -> do
        imgSize <- hFileSize h
        imgData <- BL.hGetContents h
        pure $! parseImage imgName (fromInteger imgSize) imgData

    runTest (tests img)
