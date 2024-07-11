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

  describe "intP" $ do
    it "can parse a positive number" $ do
      parseProgram (fromReadable "sssttn") `shouldBe` Right (Program [CmdStack (CmdStackPush 3)])
    it "can parse a negative number" $ do
      parseProgram (fromReadable "sstttn") `shouldBe` Right (Program [CmdStack (CmdStackPush (-3))])
    it "can parse zero" $ do
      parseProgram (fromReadable "ssssn") `shouldBe` Right (Program [CmdStack (CmdStackPush 0)])
    it "can parse a negative zero" $ do
      parseProgram (fromReadable "sstsn") `shouldBe` Right (Program [CmdStack (CmdStackPush 0)])
    it "ignores leading zeros" $ do
      parseProgram (fromReadable "sssssstn") `shouldBe` Right (Program [CmdStack (CmdStackPush 1)])
    it "fails on number without sign bit" $ do
      parseProgram (fromReadable "ssn") `shouldBe` Left "endOfInput"
    it "fails on number without terminating new line feed" $ do
      parseProgram (fromReadable "sss") `shouldBe` Left "endOfInput"

  describe "labelP" $ do
    it "can parse a label" $ do
      parseProgram (fromReadable "nssstn") `shouldBe` Right (Program [CmdFlow (CmdFlowMark "st")])
    it "can parse multiple labels" $ do
      parseProgram (fromReadable "nssstnnsstsn") `shouldBe` Right (Program [CmdFlow (CmdFlowMark "st"), CmdFlow (CmdFlowMark "ts")])
    it "fails on label without terminating new line feed" $ do
      parseProgram (fromReadable "nsss") `shouldBe` Left "endOfInput"

  describe "cmdStackP" $ do
    it "can parse CmdStackPush" $ do
      parseProgram (fromReadable "sssttn") `shouldBe` Right (Program [CmdStack (CmdStackPush 3)])
    it "can parse CmdStackDup" $ do
      parseProgram (fromReadable "stssttn") `shouldBe` Right (Program [CmdStack (CmdStackDup 3)])
    it "can parse CmdStackDiscard" $ do
      parseProgram (fromReadable "stnsttn") `shouldBe` Right (Program [CmdStack (CmdStackDiscard 3)])
    it "can parse CmdStackDupTop" $ do
      parseProgram (fromReadable "sns") `shouldBe` Right (Program [CmdStack CmdStackDupTop])
    it "can parse CmdStackSwap" $ do
      parseProgram (fromReadable "snt") `shouldBe` Right (Program [CmdStack CmdStackSwap])
    it "can parse CmdStackDiscardTop" $ do
      parseProgram (fromReadable "snn") `shouldBe` Right (Program [CmdStack CmdStackDiscardTop])

  describe "cmdArithP" $ do
    it "can parse CmdArithAdd" $ do
      parseProgram (fromReadable "tsss") `shouldBe` Right (Program [CmdArith CmdArithAdd])
    it "can parse CmdArithSub" $ do
      parseProgram (fromReadable "tsst") `shouldBe` Right (Program [CmdArith CmdArithSub])
    it "can parse CmdArithMul" $ do
      parseProgram (fromReadable "tssn") `shouldBe` Right (Program [CmdArith CmdArithMul])
    it "can parse CmdArithDiv" $ do
      parseProgram (fromReadable "tsts") `shouldBe` Right (Program [CmdArith CmdArithDiv])
    it "can parse CmdArithMod" $ do
      parseProgram (fromReadable "tstt") `shouldBe` Right (Program [CmdArith CmdArithMod])

  describe "cmdHeapP" $ do
    it "can parse CmdHeapStore" $ do
      parseProgram (fromReadable "tts") `shouldBe` Right (Program [CmdHeap CmdHeapStore])
    it "can parse CmdHeapLoad" $ do
      parseProgram (fromReadable "ttt") `shouldBe` Right (Program [CmdHeap CmdHeapLoad])

  describe "cmdIOP" $ do
    it "can parse CmdIOPrintChar" $ do
      parseProgram (fromReadable "tnss") `shouldBe` Right (Program [CmdIO CmdIOPrintChar])
    it "can parse CmdIOPrintNum" $ do
      parseProgram (fromReadable "tnst") `shouldBe` Right (Program [CmdIO CmdIOPrintNum])
    it "can parse CmdIOReadChar" $ do
      parseProgram (fromReadable "tnts") `shouldBe` Right (Program [CmdIO CmdIOReadChar])
    it "can parse CmdIOReadNum" $ do
      parseProgram (fromReadable "tntt") `shouldBe` Right (Program [CmdIO CmdIOReadNum])

  describe "cmdFlowP" $ do
    it "can parse CmdFlowMark" $ do
      parseProgram (fromReadable "nssstn") `shouldBe` Right (Program [CmdFlow (CmdFlowMark "st")])
    it "can parse CmdFlowSub" $ do
      parseProgram (fromReadable "nststn") `shouldBe` Right (Program [CmdFlow (CmdFlowSub "st")])
    it "can parse CmdFlowJump" $ do
      parseProgram (fromReadable "nsnstn") `shouldBe` Right (Program [CmdFlow (CmdFlowJump "st")])
    it "can parse CmdFlowMark" $ do
      parseProgram (fromReadable "ntsstn") `shouldBe` Right (Program [CmdFlow (CmdFlowJumpIfZero "st")])
    it "can parse CmdFlowMark" $ do
      parseProgram (fromReadable "nttstn") `shouldBe` Right (Program [CmdFlow (CmdFlowJumpIfNeg "st")])
    it "can parse CmdFlowRet" $ do
      parseProgram (fromReadable "ntn") `shouldBe` Right (Program [CmdFlow CmdFlowRet])
    it "can parse CmdFlowExit" $ do
      parseProgram (fromReadable "nnn") `shouldBe` Right (Program [CmdFlow CmdFlowExit])
