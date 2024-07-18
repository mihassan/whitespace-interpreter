module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_WhitespaceInterpreter (version)
import System.IO
import Whitespace

data Command
  = Run String

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
    versionOption :: Parser (a -> a)
    versionOption = infoOption (showVersion version) (long "version" <> help "Show version")
    programOptions :: Parser Command
    programOptions = hsubparser $ runCommand
    runCommand :: Mod CommandFields Command
    runCommand = command "run" (info runOptions (progDesc "Run a whitespace program."))
    runOptions :: Parser Command
    runOptions = Run <$> strArgument (metavar "FILE" <> help "Whitespace source file")

main :: IO ()
main = do
  Run file <- execParser optsParser
  code <- readFile file
  input <- getContents
  case whitespace code input of
    Left err -> hPutStrLn stderr $ "Error running code: " <> err
    Right output -> putStrLn $ "Successfully ran the code. Output: " <> output
