{-# LANGUAGE BlockArguments #-}

module Main where

--------------------------------------------------------------------------------

import Chkfs
import System.Environment
import System.IO

--------------------------------------------------------------------------------

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs

  case args of
    [] -> do
      hPutStrLn stderr $ "Usage: " ++ prog ++ " FILE"
      exitFailure
    imgName : _ ->
      chkfs imgName >>= bool exitFailure exitSuccess
