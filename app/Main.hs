{-# LANGUAGE BlockArguments #-}

module Main where

--------------------------------------------------------------------------------

import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           System.Environment
import           System.Exit
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
    imgOrErr <- withBinaryFile imgName ReadMode \h -> do
        imgSize <- hFileSize h
        imgData <- BL.hGetContents h
        pure $! parseImage imgName (fromInteger imgSize) imgData

    case imgOrErr of
        Right img ->
            runTest (tests img)

        Left err -> do
            hPutStrLn stderr err
            exitFailure
