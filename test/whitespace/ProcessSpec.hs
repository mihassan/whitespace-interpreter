{-# LANGUAGE OverloadedLists #-}

module Whitespace.ProcessSpec where

import Common.Util
import SpecHelper
import Whitespace.Process
import Whitespace.Program

emptyProcess :: Process
emptyProcess = Process (Program []) "" [] [] [] [] [] 0 Running

spec :: Spec
spec = do
  describe "process" $ do
    it "can create a minimal Process" $ do
      process "hello" (Program [EXIT]) `shouldBe` Right (Process (Program [EXIT]) "hello" [] [] [] [] [] 0 Running)
    it "can create a Process with labels" $ do
      process "hello" (Program [MARK "label", EXIT]) `shouldBe` Right (Process (Program [MARK "label", EXIT]) "hello" [] [] [] [("label", 0)] [] 0 Running)

  describe "running" $ do
    it "starts with the status Running" $ do
      (process "hello" (Program [EXIT]) |>> running) `shouldBe` Right True

  describe "exit" $ do
    it "is Finished after an exit instruction" $ do
      (emptyProcess |> exit |> running) `shouldBe` False

  describe "incIp" $ do
    it "increments the ip" $ do
      let p = emptyProcess {ip = 0, program = Program [PUSH 0, EXIT]}
      (p |> incIp |>> ip) `shouldBe` Right 1
    it "errors if the ip is out of bounds" $ do
      (emptyProcess |> incIp) `shouldBe` Left "Instruction pointer out of bounds"

  describe "instruction" $ do
    it "returns the instruction at the current ip" $ do
      let p = emptyProcess {ip = 0, program = Program [PUSH 0, EXIT]}
      (p |> incIp >>= instruction) `shouldBe` Right (EXIT)

  describe "push" $ do
    it "pushes a value onto the stack" $ do
      (emptyProcess |> flip push 1 |> stack) `shouldBe` [1]
    it "pushes multiple values onto the stack" $ do
      (emptyProcess |> flip push 1 |> flip push 2 |> stack) `shouldBe` [2, 1]

  describe "pop" $ do
    it "pops a value from the stack" $ do
      let p = emptyProcess {stack = [1]}
      (p |> pop |>> fst) `shouldBe` Right 1
    it "pops multiple values from the stack" $ do
      let p = emptyProcess {stack = [1, 2]}
      (p |> pop |>> snd >>= pop |>> fst) `shouldBe` Right 2
    it "errors if the stack is empty" $ do
      (emptyProcess |> pop) `shouldBe` Left "Stack underflow"

  describe "get" $ do
    it "gets the first value from the stack" $ do
      let p = emptyProcess {stack = [1, 2]}
      (p |> flip get 0) `shouldBe` Right 1
    it "gets the second value from the stack" $ do
      let p = emptyProcess {stack = [1, 2]}
      (p |> flip get 1) `shouldBe` Right 2
    it "gets the last value from the stack" $ do
      let p = emptyProcess {stack = [1, 2, 3]}
      (p |> flip get 2) `shouldBe` Right 3
    it "errors if the stack is out of bounds" $ do
      (emptyProcess |> flip get 0) `shouldBe` Left "Stack out of bounds: 0"

  describe "swap" $ do
    it "swaps the top two values on the stack" $ do
      let p = emptyProcess {stack = [1, 2]}
      (p |> swap |>> stack) `shouldBe` Right [2, 1]
    it "errors if the stack is empty" $ do
      (emptyProcess |> swap) `shouldBe` Left "Stack underflow"
    it "errors if the stack has only one element" $ do
      (emptyProcess {stack = [1]} |> swap) `shouldBe` Left "Stack underflow"

  describe "binOpE" $ do
    it "adds the top two values on the stack" $ do
      let p = emptyProcess {stack = [1, 2]}
      (p |> flip binOpE (pure ... (+)) |>> stack) `shouldBe` Right [3]
    it "errors if the stack is empty" $ do
      (emptyProcess |> flip binOpE (pure ... (+))) `shouldBe` Left "Stack underflow"
    it "errors if the stack has only one element" $ do
      (emptyProcess {stack = [1]} |> flip binOpE (pure ... (+))) `shouldBe` Left "Stack underflow"

  describe "discard" $ do
    it "discards 1 element below the top of the stack" $ do
      let p = emptyProcess {stack = [1, 2, 3]}
      (p |> flip discard 1 |>> stack) `shouldBe` Right [1, 3]
    it "discards 2 elements below the top of the stack" $ do
      let p = emptyProcess {stack = [1, 2, 3, 4]}
      (p |> flip discard 2 |>> stack) `shouldBe` Right [1, 4]
    it "does nothing for stack with one element" $ do
      (emptyProcess {stack = [1]} |> flip discard 1 |>> stack) `shouldBe` Right [1]
    it ("does nothing when n = 0") $ do
      let p = emptyProcess {stack = [1, 2, 3]}
      (p |> flip discard 0 |>> stack) `shouldBe` Right [1, 2, 3]
    it "does nothing when n = 0 from empty stack" $ do
      (emptyProcess |> flip discard 0 |>> stack) `shouldBe` Right []
    it "discards all elements except the top when n < 0" $ do
      let p = emptyProcess {stack = [1, 2, 3]}
      (p |> flip discard (-1) |>> stack) `shouldBe` Right [1]
    it "errors if the stack is empty" $ do
      (emptyProcess |> flip discard 1) `shouldBe` Left "Stack underflow"

  describe "store" $ do
    it "stores a value in the heap" $ do
      (emptyProcess |> (\p -> store p 0 1) |> heap) `shouldBe` [(0, 1)]
    it "stores multiple values in the heap" $ do
      (emptyProcess |> (\p -> store p 0 1) |> (\p -> store p 1 2) |> heap) `shouldBe` [(0, 1), (1, 2)]
    it "overwrites a value in the heap" $ do
      (emptyProcess |> (\p -> store p 0 1) |> (\p -> store p 0 2) |> heap) `shouldBe` [(0, 2)]

  describe "load" $ do
    it "loads a value from stack and push it to stack" $ do
      let p = emptyProcess {heap = [(0, 1)]}
      (p |> flip load 0 |>> stack) `shouldBe` Right [1]
    it "errors if the heap is empty" $ do
      (emptyProcess |> flip load 0) `shouldBe` Left "Heap address not found: 0"
    it "errors if the heap does not contain any value at address" $ do
      let p = emptyProcess {heap = [(1, 1)]}
      (p |> flip load 0) `shouldBe` Left "Heap address not found: 0"

  describe "readInt" $ do
    it "reads an integer from input" $ do
      let p = emptyProcess {input = "1\n2\n"}
      (p |> readInt |>> fst) `shouldBe` Right 1
    it "reads an integer from input and updates input" $ do
      let p = emptyProcess {input = "1\n2\n"}
      (p |> readInt |>> snd >>= readInt |>> fst) `shouldBe` Right 2
    it "errors if the input is empty" $ do
      (emptyProcess |> readInt) `shouldBe` Left "Empty input"
    it "errors if the input is not a number" $ do
      (emptyProcess {input = "a\n"} |> readInt) `shouldBe` Left "Not a number: a"

  describe "readChar" $ do
    it "reads a character from input" $ do
      let p = emptyProcess {input = "a"}
      (p |> readChar |>> fst) `shouldBe` Right 'a'
    it "reads a character from input and updates input" $ do
      let p = emptyProcess {input = "ab"}
      (p |> readChar |>> snd >>= readChar |>> fst) `shouldBe` Right 'b'
    it "errors if the input is empty" $ do
      (emptyProcess |> readChar) `shouldBe` Left "Empty input"

  describe "write" $ do
    it "writes a string to output" $ do
      (emptyProcess |> flip write "hello" |> output) `shouldBe` "hello"
    it "writes multiple strings to output" $ do
      (emptyProcess |> flip write "hello" |> flip write "world" |> output) `shouldBe` "helloworld"

  describe "output" $ do
    it "returns the accumulated output" $ do
      let p = emptyProcess {outputAcc = ["world", "hello"]}
      (p |> output) `shouldBe` "helloworld"

  describe "jump" $ do
    it "jumps to a label defined earlier" $ do
      let p = process "" (Program [MARK "sss", JUMP "sss", EXIT])
      let p' = p >>= incIp
      (p' >>= flip jump "sss" |>> ip) `shouldBe` Right 0
    it "jumps to a label defined later" $ do
      let p = process "" (Program [JUMP "sss", MARK "sss", EXIT])
      (p >>= flip jump "sss" |>> ip) `shouldBe` Right 1
    it "jumps to the correct label" $ do
      let p = process "" (Program [JUMP "sss", MARK "ttt", MARK "sss", EXIT])
      (p >>= flip jump "sss" |>> ip) `shouldBe` Right 2
    it "errors if the label is not found" $ do
      let p = process "" (Program [JUMP "sss", EXIT])
      (p >>= flip jump "sss") `shouldBe` Left "Label not found: \"sss\""
    it "errors if the label points to out of bounds address" $ do
      let p = emptyProcess {labels = [("sss", 1)]}
      (p |> flip jump "sss") `shouldBe` Left "Jump out of bounds"

  describe "call" $ do
    it "calls a label defined earlier" $ do
      let p = process "" (Program [MARK "sss", CALL "sss", EXIT])
      let p' = p >>= incIp
      (p' >>= flip call "sss" |>> ip) `shouldBe` Right 0
    it "calls a label defined later" $ do
      let p = process "" (Program [CALL "sss", MARK "sss", EXIT])
      (p >>= flip call "sss" |>> ip) `shouldBe` Right 1
    it "calls the correct label" $ do
      let p = process "" (Program [CALL "sss", MARK "ttt", MARK "sss", EXIT])
      (p >>= flip call "sss" |>> ip) `shouldBe` Right 2
    it "adds the next ip to the call stack" $ do
      let p = process "" (Program [CALL "sss", MARK "sss", EXIT])
      (p >>= flip call "sss" |>> callStack) `shouldBe` Right [1]
    it "adds the next ip to the call stack with non-empty call stack" $ do
      let p = process "" (Program [CALL "sss", MARK "sss", EXIT])
      let p' = p >>= \x -> pure $ x {callStack = [2]}
      (p' >>= flip call "sss" |>> callStack) `shouldBe` Right [1, 2]
    it "errors if the label is not found" $ do
      let p = process "" (Program [CALL "sss", EXIT])
      (p >>= flip call "sss") `shouldBe` Left "Label not found: \"sss\""
    it "errors if the label points to out of bounds address" $ do
      let p = process "" (Program [CALL "sss", EXIT])
      let p' = p |>> \x -> x {labels = [("sss", 2)]}
      (p' >>= flip call "sss") `shouldBe` Left "Subroutine out of bounds"
    it "errors if the next ip is out of bounds" $ do
      let p = process "" (Program [MARK "sss", EXIT, CALL "sss"])
      let p' = p |>> \x -> x {ip = 2}
      (p' >>= flip call "sss") `shouldBe` Left "Subroutine out of bounds when returning"

  describe "ret" $ do
    it "returns to the previous ip" $ do
      let p = process "" (Program [CALL "sss", PUSH 5, MARK "sss", RET, EXIT])
      let p' = p |>> \x -> x {callStack = [1], ip = 3}
      (p' >>= ret |>> ip) `shouldBe` Right 1
    it "errors if the call stack is empty" $ do
      (emptyProcess |> ret) `shouldBe` Left "Call stack empty"
    it "errors if the previous ip is out of bounds" $ do
      let p = emptyProcess {callStack = [1]}
      (p |> ret) `shouldBe` Left "Return address out of bounds"
