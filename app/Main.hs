{-# LANGUAGE BlockArguments #-}

module Main where

--------------------------------------------------------------------------------

import Chkfs
import Control.Monad
import System.Environment
import System.Exit
import System.IO

--------------------------------------------------------------------------------

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  when (null args) do
    hPutStrLn stderr $ "Usage: " ++ prog ++ " FILE"
    exitFailure

  let imgName = head args
  ok <- chkfs imgName
  if ok
    then exitSuccess
    else exitFailure
