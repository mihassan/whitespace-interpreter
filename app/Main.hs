module Main where

import Options.Applicative
import System.IO
import Whitespace

data Opts = Opts {file :: String}

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc
        <> progDesc "optparse example"
        <> header
          "optparse-example - a small example program for optparse-applicative"
    )
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.1.0.0" (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions = Opts <$> strOption (short 'f' <> long "file" <> metavar "FILE" <> help "File to run")

main :: IO ()
main = do
  opts <- execParser optsParser
  code <- readFile $ file opts
  input <- getContents
  case whitespace code input of
    Left err -> hPutStrLn stderr $ "Error running code: " <> err
    Right output -> putStrLn $ "Successfully ran the code. Output: " <> output
