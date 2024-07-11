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

  describe "validIndex" $ do
    it "returns True for first index" $ do
      Program [CmdFlow CmdFlowExit] `validIndex` 0 `shouldBe` True
    it "returns True for last index" $ do
      Program [CmdStack (CmdStackPush 0), CmdFlow CmdFlowExit] `validIndex` 1 `shouldBe` True
    it "returns False for empty program" $ do
      Program [] `validIndex` 0 `shouldBe` False
    it "returns False for out of bounds index" $ do
      Program [CmdFlow CmdFlowExit] `validIndex` 1 `shouldBe` False
    it "returns False for negative index" $ do
      Program [CmdFlow CmdFlowExit] `validIndex` (-1) `shouldBe` False

  describe "commandAt" $ do
    it "can return the first command" $ do
      commandAt (Program [CmdFlow CmdFlowExit]) 0 `shouldBe` Right (CmdFlow CmdFlowExit)
    it "can return the last command" $ do
      commandAt (Program [CmdStack (CmdStackPush 0), CmdFlow CmdFlowExit]) 1 `shouldBe` Right (CmdFlow CmdFlowExit)
    it "can return any command by index" $ do
      commandAt (Program [CmdStack (CmdStackPush 0), CmdStack CmdStackDupTop, CmdFlow CmdFlowExit]) 1 `shouldBe` Right (CmdStack CmdStackDupTop)
    it "fails on empty program" $ do
      commandAt (Program []) 0 `shouldBe` Left "No command at index: 0"
    it "fails on out of bounds index" $ do
      commandAt (Program [CmdFlow CmdFlowExit]) 1 `shouldBe` Left "No command at index: 1"
    it "fails on negative index" $ do
      commandAt (Program [CmdFlow CmdFlowExit]) (-1) `shouldBe` Left "No command at index: -1"

  describe "findLabels" $ do
    it "can find a single label" $ do
      findLabels (Program [CmdFlow (CmdFlowMark "label"), CmdFlow CmdFlowExit]) `shouldBe` Right [("label", 0)]
    it "can find multiple labels" $ do
      findLabels (Program [CmdFlow (CmdFlowMark "label1"), CmdFlow (CmdFlowMark "label2"), CmdFlow CmdFlowExit]) `shouldBe` Right [("label1", 0), ("label2", 1)]
    it "finds no labels in an empty program" $ do
      findLabels (Program []) `shouldBe` Right []
    it "finds no labels in a program with no labels" $ do
      findLabels (Program [CmdFlow CmdFlowExit]) `shouldBe` Right []
    it "ignores other commands with labels" $ do
      findLabels (Program [CmdFlow (CmdFlowJump "label"), CmdStack (CmdStackPush 0), CmdFlow CmdFlowExit]) `shouldBe` Right []
    it "ignores duplicate labels for non-mark commands" $ do
      findLabels (Program [CmdFlow (CmdFlowJump "label"), CmdFlow (CmdFlowJump "label"), CmdFlow CmdFlowExit]) `shouldBe` Right []
    it "fails on duplicate labels" $ do
      findLabels (Program [CmdFlow (CmdFlowMark "label"), CmdFlow (CmdFlowMark "label"), CmdFlow CmdFlowExit]) `shouldBe` Left "Duplicate labels found"
