module SpecHelper (module Test.Hspec, fromReadable, toReadable) where

import Common.Util
import Test.Hspec
import Whitespace.Converter

fromReadable :: String -> String
fromReadable code =
  convert (Params {from = Readable, to = Whitespace}) code |> \case
    Left err -> error err
    Right x -> x

toReadable :: String -> String
toReadable code =
  convert (Params {from = Whitespace, to = Readable}) code |> \case
    Left err -> error err
    Right x -> x
