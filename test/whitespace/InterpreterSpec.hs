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
      let p = emptyProcess {program = Program [EXIT]}
      runProcess p `shouldBe` Right p {status = Finished}
    it "can run a process and generate output" $ do
      let p = emptyProcess {program = Program [PUSH 65, WRITE_CHAR, EXIT]}
      (runProcess p |>> output) `shouldBe` Right "A"
    it "can run a process and read input" $ do
      let p = emptyProcess {program = Program [PUSH 0, DUP_TOP, READ_CHAR, LOAD, WRITE_CHAR, EXIT], input = "A"}
      (runProcess p |>> output) `shouldBe` Right "A"
    it "can throw an error when trying to divide by zero" $ do
      let p = emptyProcess {program = Program [PUSH 1, PUSH 0, DIV, EXIT]}
      runProcess p `shouldBe` Left "Division by zero"
    it "can run stack commands" $ do
      let p = emptyProcess {program = Program [PUSH 1, PUSH 2, DUP 1, DISCARD 1, DUP_TOP, DISCARD_TOP, DISCARD 1, EXIT]}
      (runProcess p |>> stack) `shouldBe` Right [1]
    it "can run arithmetic commands" $ do
      let p = emptyProcess {program = Program [PUSH 1, PUSH 2, ADD, PUSH 1, SUB, PUSH 3, MUL, PUSH 2, DIV, PUSH 2, MOD, EXIT]}
      (runProcess p |>> stack) `shouldBe` Right [1]
    it "can run heap commands" $ do
      let p = emptyProcess {program = Program [PUSH 1, PUSH 2, STORE, PUSH 1, LOAD, EXIT]}
      (runProcess p |>> stack) `shouldBe` Right [2]
    it "can run IO commands" $ do
      let p = emptyProcess {program = Program [PUSH 65, WRITE_CHAR, PUSH 65, WRITE_NUM, EXIT]}
      (runProcess p |>> output) `shouldBe` Right "A65"
    it "can run flow commands" $ do
      let p = emptyProcess {program = Program [CALL "sss", MARK "sss", EXIT], labels = [("sss", 1)]}
      runProcess p `shouldBe` Right p {status = Finished, ip = 2, callStack = [1]}
