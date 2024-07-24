{-# LANGUAGE OverloadedLists #-}

module Whitespace.ProgramSpec where

import SpecHelper
import Whitespace.Program

spec :: Spec
spec = do
  describe "parseProgram" $ do
    it "can parse a minimal program" $ do
      parseProgram "\n\n\n" `shouldBe` Right (Program [EXIT])
    it "can parse a minimal program with comments" $ do
      parseProgram "abcABC123-_@\n\n\n" `shouldBe` Right (Program [EXIT])
    it "can parse an empty program" $ do
      parseProgram "" `shouldBe` Right (Program [])
    it "fails to parse an invalid program" $ do
      parseProgram " " `shouldBe` Left "endOfInput"

  describe "showProgram" $ do
    it "can show a minimal program" $ do
      showProgram (Program [EXIT]) `shouldBe` "\n\n\n"
    it "can show an empty program" $ do
      showProgram (Program []) `shouldBe` ""

  describe "validIndex" $ do
    it "returns True for first index" $ do
      Program [EXIT] `validIndex` 0 `shouldBe` True
    it "returns True for last index" $ do
      Program [PUSH 0, EXIT] `validIndex` 1 `shouldBe` True
    it "returns False for empty program" $ do
      Program [] `validIndex` 0 `shouldBe` False
    it "returns False for out of bounds index" $ do
      Program [EXIT] `validIndex` 1 `shouldBe` False
    it "returns False for negative index" $ do
      Program [EXIT] `validIndex` (-1) `shouldBe` False

  describe "fromInstructions" $ do
    it "can create a program from a list of instructions" $ do
      fromInstructions [PUSH 0, EXIT] `shouldBe` Program [PUSH 0, EXIT]

  describe "instructions" $ do
    it "can return the instructions of a program" $ do
      instructions (Program [PUSH 0, EXIT]) `shouldBe` [PUSH 0, EXIT]

  describe "instructionAt" $ do
    it "can return the first instruction" $ do
      instructionAt (Program [EXIT]) 0 `shouldBe` Right (EXIT)
    it "can return the last instruction" $ do
      instructionAt (Program [PUSH 0, EXIT]) 1 `shouldBe` Right (EXIT)
    it "can return any instruction by index" $ do
      instructionAt (Program [PUSH 0, DUP_TOP, EXIT]) 1 `shouldBe` Right (DUP_TOP)
    it "fails on empty program" $ do
      instructionAt (Program []) 0 `shouldBe` Left "No instruction at index: 0"
    it "fails on out of bounds index" $ do
      instructionAt (Program [EXIT]) 1 `shouldBe` Left "No instruction at index: 1"
    it "fails on negative index" $ do
      instructionAt (Program [EXIT]) (-1) `shouldBe` Left "No instruction at index: -1"

  describe "findLabels" $ do
    it "can find a single label" $ do
      findLabels (Program [MARK "label", EXIT]) `shouldBe` Right [("label", 0)]
    it "can find multiple labels" $ do
      findLabels (Program [MARK "label1", MARK "label2", EXIT]) `shouldBe` Right [("label1", 0), ("label2", 1)]
    it "can find a label in the middle of the program" $ do
      findLabels (Program [PUSH 0, MARK "label", EXIT]) `shouldBe` Right [("label", 1)]
    it "finds no labels in an empty program" $ do
      findLabels (Program []) `shouldBe` Right []
    it "finds no labels in a program with no labels" $ do
      findLabels (Program [EXIT]) `shouldBe` Right []
    it "ignores other commands with labels" $ do
      findLabels (Program [JUMP "label", PUSH 0, EXIT]) `shouldBe` Right []
    it "ignores duplicate labels for non-mark commands" $ do
      findLabels (Program [JUMP "label", JUMP "label", EXIT]) `shouldBe` Right []
    it "fails on duplicate labels" $ do
      findLabels (Program [MARK "label", MARK "label", EXIT]) `shouldBe` Left "Duplicate labels found"

  describe "showInt" $ do
    it "can show a positive number" $ do
      showProgram (Program [PUSH 3]) `shouldBe` (fromReadable "sssttn")
    it "can show a negative number" $ do
      showProgram (Program [PUSH (-3)]) `shouldBe` (fromReadable "sstttn")
    it "can show zero" $ do
      showProgram (Program [PUSH 0]) `shouldBe` (fromReadable "sssn")

  describe "showLabel" $ do
    it "can show a label" $ do
      showProgram (Program [MARK "st"]) `shouldBe` (fromReadable "nssstn")
    it "can show multiple labels" $ do
      showProgram (Program [MARK "st", MARK "ts"]) `shouldBe` (fromReadable "nssstnnsstsn")

  describe "showInstruction" $ do
    it "can show PUSH" $ do
      showProgram (Program [PUSH 3]) `shouldBe` (fromReadable "sssttn")
    it "can show DUP" $ do
      showProgram (Program [DUP 3]) `shouldBe` (fromReadable "stssttn")
    it "can show DISCARD" $ do
      showProgram (Program [DISCARD 3]) `shouldBe` (fromReadable "stnsttn")
    it "can show DUP_TOP" $ do
      showProgram (Program [DUP_TOP]) `shouldBe` (fromReadable "sns")
    it "can show SWAP" $ do
      showProgram (Program [SWAP]) `shouldBe` (fromReadable "snt")
    it "can show DISCARD_TOP" $ do
      showProgram (Program [DISCARD_TOP]) `shouldBe` (fromReadable "snn")
    it "can show ADD" $ do
      showProgram (Program [ADD]) `shouldBe` (fromReadable "tsss")
    it "can show SUB" $ do
      showProgram (Program [SUB]) `shouldBe` (fromReadable "tsst")
    it "can show MUL" $ do
      showProgram (Program [MUL]) `shouldBe` (fromReadable "tssn")
    it "can show DIV" $ do
      showProgram (Program [DIV]) `shouldBe` (fromReadable "tsts")
    it "can show MOD" $ do
      showProgram (Program [MOD]) `shouldBe` (fromReadable "tstt")
    it "can show STORE" $ do
      showProgram (Program [STORE]) `shouldBe` (fromReadable "tts")
    it "can show LOAD" $ do
      showProgram (Program [LOAD]) `shouldBe` (fromReadable "ttt")
    it "can show WRITE_CHAR" $ do
      showProgram (Program [WRITE_CHAR]) `shouldBe` (fromReadable "tnss")
    it "can show WRITE_NUM" $ do
      showProgram (Program [WRITE_NUM]) `shouldBe` (fromReadable "tnst")
    it "can show READ_CHAR" $ do
      showProgram (Program [READ_CHAR]) `shouldBe` (fromReadable "tnts")
    it "can show READ_NUM" $ do
      showProgram (Program [READ_NUM]) `shouldBe` (fromReadable "tntt")
    it "can show MARK" $ do
      showProgram (Program [MARK "st"]) `shouldBe` (fromReadable "nssstn")
    it "can show CALL" $ do
      showProgram (Program [CALL "st"]) `shouldBe` (fromReadable "nststn")
    it "can show JUMP" $ do
      showProgram (Program [JUMP "st"]) `shouldBe` (fromReadable "nsnstn")
    it "can show JZ" $ do
      showProgram (Program [JZ "st"]) `shouldBe` (fromReadable "ntsstn")
    it "can show JNG" $ do
      showProgram (Program [JNG "st"]) `shouldBe` (fromReadable "nttstn")
    it "can show RET" $ do
      showProgram (Program [RET]) `shouldBe` (fromReadable "ntn")
    it "can show EXIT" $ do
      showProgram (Program [EXIT]) `shouldBe` (fromReadable "nnn")

  describe "intP" $ do
    it "can parse a positive number" $ do
      parseProgram (fromReadable "sssttn") `shouldBe` Right (Program [PUSH 3])
    it "can parse a negative number" $ do
      parseProgram (fromReadable "sstttn") `shouldBe` Right (Program [PUSH (-3)])
    it "can parse zero" $ do
      parseProgram (fromReadable "sssn") `shouldBe` Right (Program [PUSH 0])
    it "can parse a negative zero" $ do
      parseProgram (fromReadable "sstn") `shouldBe` Right (Program [PUSH 0])
    it "ignores leading zeros" $ do
      parseProgram (fromReadable "sssssstn") `shouldBe` Right (Program [PUSH 1])
    it "fails on number without sign bit" $ do
      parseProgram (fromReadable "ssn") `shouldBe` Left "endOfInput"
    it "fails on number without terminating new line feed" $ do
      parseProgram (fromReadable "sss") `shouldBe` Left "endOfInput"

  describe "labelP" $ do
    it "can parse a label" $ do
      parseProgram (fromReadable "nssstn") `shouldBe` Right (Program [MARK "st"])
    it "can parse multiple labels" $ do
      parseProgram (fromReadable "nssstnnsstsn") `shouldBe` Right (Program [MARK "st", MARK "ts"])
    it "fails on label without terminating new line feed" $ do
      parseProgram (fromReadable "nsss") `shouldBe` Left "endOfInput"

  describe "stackP" $ do
    it "can parse PUSH" $ do
      parseProgram (fromReadable "sssttn") `shouldBe` Right (Program [PUSH 3])
    it "can parse DUP" $ do
      parseProgram (fromReadable "stssttn") `shouldBe` Right (Program [DUP 3])
    it "can parse DISCARD" $ do
      parseProgram (fromReadable "stnsttn") `shouldBe` Right (Program [DISCARD 3])
    it "can parse DUP_TOP" $ do
      parseProgram (fromReadable "sns") `shouldBe` Right (Program [DUP_TOP])
    it "can parse SWAP" $ do
      parseProgram (fromReadable "snt") `shouldBe` Right (Program [SWAP])
    it "can parse DISCARD_TOP" $ do
      parseProgram (fromReadable "snn") `shouldBe` Right (Program [DISCARD_TOP])

  describe "arithP" $ do
    it "can parse ADD" $ do
      parseProgram (fromReadable "tsss") `shouldBe` Right (Program [ADD])
    it "can parse SUB" $ do
      parseProgram (fromReadable "tsst") `shouldBe` Right (Program [SUB])
    it "can parse MUL" $ do
      parseProgram (fromReadable "tssn") `shouldBe` Right (Program [MUL])
    it "can parse DIV" $ do
      parseProgram (fromReadable "tsts") `shouldBe` Right (Program [DIV])
    it "can parse MOD" $ do
      parseProgram (fromReadable "tstt") `shouldBe` Right (Program [MOD])

  describe "heapP" $ do
    it "can parse STORE" $ do
      parseProgram (fromReadable "tts") `shouldBe` Right (Program [STORE])
    it "can parse LOAD" $ do
      parseProgram (fromReadable "ttt") `shouldBe` Right (Program [LOAD])

  describe "ioP" $ do
    it "can parse WRITE_CHAR" $ do
      parseProgram (fromReadable "tnss") `shouldBe` Right (Program [WRITE_CHAR])
    it "can parse WRITE_NUM" $ do
      parseProgram (fromReadable "tnst") `shouldBe` Right (Program [WRITE_NUM])
    it "can parse READ_CHAR" $ do
      parseProgram (fromReadable "tnts") `shouldBe` Right (Program [READ_CHAR])
    it "can parse READ_NUM" $ do
      parseProgram (fromReadable "tntt") `shouldBe` Right (Program [READ_NUM])

  describe "flowP" $ do
    it "can parse MARK" $ do
      parseProgram (fromReadable "nssstn") `shouldBe` Right (Program [MARK "st"])
    it "can parse CALL" $ do
      parseProgram (fromReadable "nststn") `shouldBe` Right (Program [CALL "st"])
    it "can parse JUMP" $ do
      parseProgram (fromReadable "nsnstn") `shouldBe` Right (Program [JUMP "st"])
    it "can parse JZ" $ do
      parseProgram (fromReadable "ntsstn") `shouldBe` Right (Program [JZ "st"])
    it "can parse JNG" $ do
      parseProgram (fromReadable "nttstn") `shouldBe` Right (Program [JNG "st"])
    it "can parse RET" $ do
      parseProgram (fromReadable "ntn") `shouldBe` Right (Program [RET])
    it "can parse EXIT" $ do
      parseProgram (fromReadable "nnn") `shouldBe` Right (Program [EXIT])
