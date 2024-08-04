{-# LANGUAGE RecordWildCards #-}

module Whitespace.Converter (Format (..), Params (..), convert) where

import Common.Util
import Data.Maybe
import Text.Read
import Whitespace.Program

data Format = Whitespace | Readable | Interpreted deriving (Eq, Show, Read)

data Params = Params
  { from :: Format,
    to :: Format
  }
  deriving (Eq, Show, Read)

convert :: Params -> String -> Either String String
convert (Params {..}) code = case (from, to) of
  (Whitespace, Readable) -> Right $ whitespaceToReadable code
  (Whitespace, Interpreted) -> whitespaceToInterpreted code
  (Readable, Whitespace) -> Right $ readableToWhitespace code
  (Interpreted, Whitespace) -> interpretedToWhitespace code
  (Whitespace, Whitespace) -> Right code
  (Readable, Readable) -> Right code
  (Interpreted, Interpreted) -> Right code
  _ -> Left "Invalid conversion"

readableToWhitespace :: String -> String
readableToWhitespace = mapMaybe go
  where
    go :: Char -> Maybe Char
    go 's' = Just ' '
    go 't' = Just '\t'
    go 'n' = Just '\n'
    go _ = Nothing

whitespaceToReadable :: String -> String
whitespaceToReadable = mapMaybe go
  where
    go :: Char -> Maybe Char
    go ' ' = Just 's'
    go '\t' = Just 't'
    go '\n' = Just 'n'
    go _ = Nothing

whitespaceToInterpreted :: String -> Either String String
whitespaceToInterpreted code = (unlines . map show . instructions) <$> parseProgram code

interpretedToWhitespace :: String -> Either String String
interpretedToWhitespace code = do
  cmds <- lines code |> traverse readInstruction
  pure $ cmds |> fromInstructions |> showProgram
  where
    readInstruction :: String -> Either String Instruction
    readInstruction x = readMaybe x |> maybeToEither ("Invalid instruction: " <> x)
