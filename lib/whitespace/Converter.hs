{-# LANGUAGE RecordWildCards #-}

module Whitespace.Converter (Format (..), Params (..), convert) where

import Common.Util
import Data.Maybe
import Text.Read
import Whitespace.Program

data Format = Original | Readable | Runnable deriving (Eq, Show, Read)

data Params = Params
  { from :: Format,
    to :: Format
  }
  deriving (Eq, Show, Read)

convert :: Params -> String -> Either String String
convert (Params {..}) code = case (from, to) of
  (Original, Readable) -> Right $ originalToReadable code
  (Original, Runnable) -> originalToRunnable code
  (Readable, Original) -> Right $ readableToOriginal code
  (Runnable, Original) -> runnableToOriginal code
  (Original, Original) -> Right code
  (Readable, Readable) -> Right code
  (Runnable, Runnable) -> Right code
  _ -> Left "Invalid conversion"

readableToOriginal :: String -> String
readableToOriginal = mapMaybe go
  where
    go :: Char -> Maybe Char
    go 's' = Just ' '
    go 't' = Just '\t'
    go 'n' = Just '\n'
    go _ = Nothing

originalToReadable :: String -> String
originalToReadable = mapMaybe go
  where
    go :: Char -> Maybe Char
    go ' ' = Just 's'
    go '\t' = Just 't'
    go '\n' = Just 'n'
    go _ = Nothing

originalToRunnable :: String -> Either String String
originalToRunnable code = (unlines . map show . instructions) <$> parseProgram code

runnableToOriginal :: String -> Either String String
runnableToOriginal code = do
  cmds <- lines code |> traverse readInstruction
  pure $ cmds |> fromInstructions |> showProgram
  where
    readInstruction :: String -> Either String Instruction
    readInstruction x = readMaybe x |> maybeToEither ("Invalid instruction: " <> x)
