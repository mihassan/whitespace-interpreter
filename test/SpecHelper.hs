module SpecHelper (module Test.Hspec, fromReadable, toReadable) where

import Test.Hspec
import Whitespace.Converter

fromReadable :: String -> String
fromReadable = readableToOriginal

toReadable :: String -> String
toReadable = originalToReadable
