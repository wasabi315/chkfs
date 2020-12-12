{-# LANGUAGE BlockArguments #-}

module Main where

--------------------------------------------------------------------------------

import           Control.Monad
import           System.Environment
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
    mmapWithFilePtr imgName ReadOnly Nothing \(img, _) -> do
        sb <- getSuperblock img
        runTest (superblockTT sb)
