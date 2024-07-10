{-# LANGUAGE OverloadedLists #-}

module Whitespace.ProgramSpec where

import SpecHelper
import Whitespace.Program

spec :: Spec
spec = do
  describe "parseProgram" $ do
    it "can parse a minimal program" $ do
      parseProgram "\n\n\n" `shouldBe` Right (Program [CmdFlow CmdFlowExit])
    it "can parse a minimal program with comments" $ do
      parseProgram "abcABC123-_@\n\n\n" `shouldBe` Right (Program [CmdFlow CmdFlowExit])
    it "can parse an empty program" $ do
      parseProgram "" `shouldBe` Right (Program [])
    it "fails to parse an invalid program" $ do
      parseProgram " " `shouldBe` Left "endOfInput"
