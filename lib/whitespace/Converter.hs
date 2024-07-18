module Whitespace.Converter (convertFromReadable, convertToReadable) where

import Data.Maybe

convertFromReadable :: String -> String
convertFromReadable = mapMaybe go
  where
    go :: Char -> Maybe Char
    go 's' = Just ' '
    go 't' = Just '\t'
    go 'n' = Just '\n'
    go _ = Nothing

convertToReadable :: String -> String
convertToReadable = mapMaybe go
  where
    go :: Char -> Maybe Char
    go ' ' = Just 's'
    go '\t' = Just 't'
    go '\n' = Just 'n'
    go _ = Nothing
