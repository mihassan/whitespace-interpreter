{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_WhitespaceInterpreter (version)
import System.IO
import Whitespace

data Format = Original | Readable | Runnable deriving (Eq, Show, Read)

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

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version) (long "version" <> short 'v' <> help "Show version")

programOptions :: Parser Command
programOptions = hsubparser (runCommand <> convertCommand)

runCommand :: Mod CommandFields Command
runCommand = command "run" (info runOptions (progDesc "Run a whitespace program."))

runOptions :: Parser Command
runOptions = Run <$> strArgument (metavar "FILE" <> help "Whitespace source file")

convertCommand :: Mod CommandFields Command
convertCommand = command "convert" (info convertOptions (progDesc "Convert a whitespace file to a different formats (i.e., Original, Readable, Runnable)."))

convertOptions :: Parser Command
convertOptions =
  Convert
    <$> option auto (long "from" <> short 'f' <> metavar "FORMAT" <> help "Format to convert from")
    <*> option auto (long "to" <> short 't' <> metavar "FORMAT" <> help "Format to convert to")
    <*> strArgument (metavar "FILE" <> help "File to convert")

main :: IO ()
main = do
  execParser optsParser >>= \case
    Run {..} -> run runFile
    Convert {..} -> convert convertFrom convertTo convertFile

run :: FilePath -> IO ()
run file = do
  code <- readFile file
  input <- getContents
  case whitespace code input of
    Left err -> hPutStrLn stderr $ "Error running code: " <> err
    Right output -> putStrLn $ "Successfully ran the code. Output: " <> output

convert :: Format -> Format -> FilePath -> IO ()
convert from to file = readFile file >>= go from to >>= putStr
  where
    go Original Readable c = pure $ originalToReadable c
    go Readable Original c = pure $ readableToOriginal c
    go Original Runnable c = case originalToRunnable c of
      Left err -> do
        let msg = "Error converting to runnable code: " <> err
        hPutStrLn stderr msg
        pure ""
      Right runnable -> pure runnable
    go Runnable Original c = case runnableToOriginal c of
      Left err -> do
        let msg = "Error converting to original code: "
        hPutStrLn stderr $ msg <> err
        pure ""
      Right original -> pure original
    go f t c | f == t = pure c
    go f t _ = do
      let msg = "Cannot convert from " <> show f <> " to " <> show t
      hPutStrLn stderr msg
      pure ""
