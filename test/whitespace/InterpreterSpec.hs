{-# LANGUAGE OverloadedLists #-}

module Whitespace.InterpreterSpec where

import Common.Util
import SpecHelper
import Whitespace.Interpreter
import Whitespace.Process
import Whitespace.Program

emptyProcess :: Process
emptyProcess = Process (Program []) "" [] [] [] [] [] 0 Running

spec :: Spec
spec = do
  describe "runProcess" $ do
    it "can run a simple process to finish" $ do
      let p = emptyProcess {program = Program [CmdFlow CmdFlowExit]}
      runProcess p `shouldBe` Right p {status = Finished}
    it "can run a process and generate output" $ do
      let p = emptyProcess {program = Program [CmdStack (CmdStackPush 65), CmdIO CmdIOPrintChar, CmdFlow CmdFlowExit]}
      (runProcess p |>> output) `shouldBe` Right "A"
    it "can run a process and read input" $ do
      let p = emptyProcess {program = Program [CmdStack (CmdStackPush 0), CmdStack CmdStackDupTop, CmdIO CmdIOReadChar, CmdHeap CmdHeapLoad, CmdIO CmdIOPrintChar, CmdFlow CmdFlowExit], input = "A"}
      (runProcess p |>> output) `shouldBe` Right "A"
    it "can throw an error when trying to divide by zero" $ do
      let p = emptyProcess {program = Program [CmdStack (CmdStackPush 1), CmdStack (CmdStackPush 0), CmdArith CmdArithDiv, CmdFlow CmdFlowExit]}
      runProcess p `shouldBe` Left "Division by zero"
    it "can run stack commands" $ do
      let p = emptyProcess {program = Program [CmdStack (CmdStackPush 1), CmdStack (CmdStackPush 2), CmdStack (CmdStackDup 1), CmdStack (CmdStackDiscard 1), CmdStack (CmdStackDupTop), CmdStack CmdStackDiscardTop, CmdStack (CmdStackDiscard 1), CmdFlow CmdFlowExit]}
      (runProcess p |>> stack) `shouldBe` Right [1]
    it "can run arithmetic commands" $ do
      let p = emptyProcess {program = Program [CmdStack (CmdStackPush 1), CmdStack (CmdStackPush 2), CmdArith CmdArithAdd, CmdStack (CmdStackPush 1), CmdArith CmdArithSub, CmdStack (CmdStackPush 3), CmdArith CmdArithMul, CmdStack (CmdStackPush 2), CmdArith CmdArithDiv, CmdStack (CmdStackPush 2), CmdArith CmdArithMod, CmdFlow CmdFlowExit]}
      (runProcess p |>> stack) `shouldBe` Right [1]
    it "can run heap commands" $ do
      let p = emptyProcess {program = Program [CmdStack (CmdStackPush 1), CmdStack (CmdStackPush 2), CmdHeap CmdHeapStore, CmdStack (CmdStackPush 1), CmdHeap CmdHeapLoad, CmdFlow CmdFlowExit]}
      (runProcess p |>> stack) `shouldBe` Right [2]
    it "can run IO commands" $ do
      let p = emptyProcess {program = Program [CmdStack (CmdStackPush 65), CmdIO CmdIOPrintChar, CmdStack (CmdStackPush 65), CmdIO CmdIOPrintNum, CmdFlow CmdFlowExit]}
      (runProcess p |>> output) `shouldBe` Right "A65"
    it "can run flow commands" $ do
      let p = emptyProcess {program = Program [CmdFlow (CmdFlowSub "sss"), CmdFlow (CmdFlowMark "sss"), CmdFlow CmdFlowExit], labels = [("sss", 1)]}
      runProcess p `shouldBe` Right p {status = Finished, ip = 2, callStack = [1]}
