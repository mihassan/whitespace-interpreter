{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_WhitespaceInterpreter (version)
import System.IO
import Whitespace

data Command
  = Run {runFile :: FilePath}
  | Convert {convertFrom :: Format, convertTo :: Format, convertFile :: FilePath}

optsParser :: ParserInfo Command
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc
        <> progDesc "Whitespace Interpreter"
        <> header
          "Whitespace Interpreter - an interpreter for the Whitespace programming language"
    )
  where
    versionOption =
      infoOption
        (showVersion version)
        (long "version" <> short 'v' <> help "Show version")
    programOptions = hsubparser (runOption <> convertOption)
    runOption =
      command "run" $
        info
          ( Run
              <$> strArgument (metavar "FILE" <> help "Whitespace source file")
          )
          (progDesc "Run a whitespace program.")
    convertOption =
      command "convert" $
        info
          ( Convert
              <$> option auto (long "from" <> short 'f' <> metavar "FORMAT" <> help "Format to convert from")
              <*> option auto (long "to" <> short 't' <> metavar "FORMAT" <> help "Format to convert to")
              <*> strArgument (metavar "FILE" <> help "File to convert")
          )
          (progDesc "Convert a whitespace file to a different formats (i.e., Whitespace, Readable, Interpreted).")

runCommand :: FilePath -> IO ()
runCommand file = do
  code <- readFile file
  input <- getContents
  case whitespace code input of
    Left err -> hPutStrLn stderr $ "Error running code: " <> err
    Right output -> putStrLn $ "Successfully ran the code. Output: " <> output

convertCommand :: Format -> Format -> FilePath -> IO ()
convertCommand from to file = do
  code <- readFile file
  let params = Params {from = from, to = to}
  case convert params code of
    Left err -> hPutStrLn stderr $ "Error converting code: " <> err
    Right output -> putStrLn output

main :: IO ()
main = do
  execParser optsParser >>= \case
    Run {..} -> runCommand runFile
    Convert {..} -> convertCommand convertFrom convertTo convertFile
