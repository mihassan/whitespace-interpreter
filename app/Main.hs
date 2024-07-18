module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_WhitespaceInterpreter (version)
import System.IO
import Whitespace

data Opts = Opts {file :: String}

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc
        <> progDesc "Whitespace Interpreter"
        <> header
          "Whitespace Interpreter - an interpreter for the Whitespace programming language"
    )
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption (showVersion version) (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions =
      Opts
        <$> strArgument (metavar "FILE" <> help "Whitespace source file")

main :: IO ()
main = do
  opts <- execParser optsParser
  code <- readFile $ file opts
  input <- getContents
  case whitespace code input of
    Left err -> hPutStrLn stderr $ "Error running code: " <> err
    Right output -> putStrLn $ "Successfully ran the code. Output: " <> output
