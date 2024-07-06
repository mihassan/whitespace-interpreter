module Main where

import Whitespace
import System.Environment
import System.IO

main :: IO ()
main = do
  [f] <- getArgs
  code <- readFile f
  input <- getContents
  case whitespace code input of
    Left err -> hPutStrLn stderr $ "Error running code: " <> err
    Right output -> putStrLn $ "Successfully ran the code. Output: " <> output
