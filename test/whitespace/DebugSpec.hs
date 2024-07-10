module Whitespace.DebugSpec where

import SpecHelper
import Whitespace.Debug

spec :: Spec
spec = do
  describe "fromReadable" $ do
    it "converts whitespace tokens" $ do
      fromReadable "stn" `shouldBe` " \t\n"
    it "ignores comment characters" $ do
      fromReadable "abcABC123-_@" `shouldBe` ""
    it "ignores original whitespace characters" $ do
      fromReadable " \t\n" `shouldBe` ""
  describe "toReadable" $ do
    it "converts whitespace characters" $ do
      toReadable " \t\n" `shouldBe` "stn"
