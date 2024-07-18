module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_WhitespaceInterpreter (version)
import System.IO
import Whitespace

data Command
  = Run FilePath
  | Convert Bool FilePath

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
convertCommand = command "convert" (info convertOptions (progDesc "Convert between a readable whitespace program and an original whitespace program."))

convertOptions :: Parser Command
convertOptions =
  Convert
    <$> switch (long "reverse" <> short 'r' <> help "Convert a whitespace program to a readable whitespace program.")
    <*> strArgument (metavar "FILE" <> help "File to convert")

main :: IO ()
main = do
  execParser optsParser >>= \case
    Run file -> run file
    Convert r file -> convert r file

run :: FilePath -> IO ()
run file = do
  code <- readFile file
  input <- getContents
  case whitespace code input of
    Left err -> hPutStrLn stderr $ "Error running code: " <> err
    Right output -> putStrLn $ "Successfully ran the code. Output: " <> output

convert :: Bool -> FilePath -> IO ()
convert toReadable file = do
  code <- readFile file
  let convertedCode = if toReadable then convertToReadable code else convertFromReadable code
  putStr convertedCode
