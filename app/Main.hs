{-# LANGUAGE BlockArguments #-}

module Main where

--------------------------------------------------------------------------------

import           Control.Monad
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.MMap

import           Chkfs

--------------------------------------------------------------------------------

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    when (null args) do
        hPutStr stderr $ "Usage: " ++ prog ++ " FILE"

    let imgName = head args
    ok <- mmapWithFilePtr imgName ReadOnly Nothing \(img, imgSize) -> do
        runTest (createTests imgName imgSize img)

    if ok
        then exitSuccess
        else exitFailure
