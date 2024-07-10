module Whitespace.Debug where

import Data.Maybe

fromReadable :: String -> String
fromReadable = mapMaybe $ \case
  's' -> Just ' '
  't' -> Just '\t'
  'n' -> Just '\n'
  _ -> Nothing

toReadable :: String -> String
toReadable = concatMap $ \case
  ' ' -> "s"
  '\t' -> "t"
  '\n' -> "n"
  _ -> error "toReadable: invalid character"
