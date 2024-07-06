module Whitespace.Program where

import Control.Applicative
import Data.Functor
import Whitespace.Parser
import Whitespace.Tokenizer (Token (..))

type Number = Int

type Label = [Token]

type Program = [Command]

data Command = CmdStack CmdStack | CmdArith CmdArith | CmdHeap CmdHeap | CmdIO CmdIO | CmdFlow CmdFlow deriving (Eq, Show)

data CmdStack = CmdStackPush Number | CmdStackDup Number | CmdStackDiscard Number | CmdStackDupTop | CmdStackSwap | CmdStackDiscardTop deriving (Eq, Show)

data CmdArith = CmdArithAdd | CmdArithSub | CmdArithMul | CmdArithDiv | CmdArithMod deriving (Eq, Show)

data CmdHeap = CmdHeapStore | CmdHeapLoad deriving (Eq, Show)

data CmdIO = CmdIOPrintChar | CmdIOPrintNum | CmdIOReadChar | CmdIOReadNum deriving (Eq, Show)

data CmdFlow = CmdFlowMark Label | CmdFlowSub Label | CmdFlowJump Label | CmdFlowJumpIfZero Label | CmdFlowJumpIfNeg Label | CmdFlowRet | CmdFlowExit deriving (Eq, Show)

type TokenParser a = Parser [Token] a

tokenP :: Token -> TokenParser Token
tokenP t = satisfy (== t)

spaceP :: TokenParser Token
spaceP = tokenP Space

tabP :: TokenParser Token
tabP = tokenP Tab

lfP :: TokenParser Token
lfP = tokenP LF

programP :: TokenParser Program
programP = many commandP <* eof

commandP :: TokenParser Command
commandP =
  CmdStack
    <$> (spaceP *> cmdStackP)
    <|> CmdArith
      <$> (tabP *> spaceP *> cmdArithP)
    <|> CmdHeap
      <$> (tabP *> tabP *> cmdHeapP)
    <|> CmdIO
      <$> (tabP *> lfP *> cmdIOP)
    <|> CmdFlow
      <$> (lfP *> cmdFlowP)

numberP :: TokenParser Number
numberP = do
  sign <- (tabP $> -1) <|> (spaceP $> 1)
  bits <- many $ (tabP $> 1) <|> (spaceP $> 0)
  lfP
  pure $ convert sign bits 0
  where
    convert s [] acc = s * acc
    convert s (x : xs) acc = convert s xs (acc * 2 + x)

labelP :: TokenParser Label
labelP = many (tabP <|> spaceP) <* lfP

cmdStackP :: TokenParser CmdStack
cmdStackP =
  spaceP
    *> (CmdStackPush <$> numberP)
    <|> tabP
      *> spaceP
      *> (CmdStackDup <$> numberP)
    <|> tabP
      *> lfP
      *> (CmdStackDiscard <$> numberP)
    <|> lfP
      *> spaceP
      $> CmdStackDupTop
    <|> lfP
      *> tabP
      $> CmdStackSwap
    <|> lfP
      *> lfP
      $> CmdStackDiscardTop

cmdArithP :: TokenParser CmdArith
cmdArithP =
  spaceP
    *> spaceP
    $> CmdArithAdd
    <|> spaceP
      *> tabP
      $> CmdArithSub
    <|> spaceP
      *> lfP
      $> CmdArithMul
    <|> tabP
      *> spaceP
      $> CmdArithDiv
    <|> tabP
      *> tabP
      $> CmdArithMod

cmdHeapP :: TokenParser CmdHeap
cmdHeapP =
  spaceP
    $> CmdHeapStore
    <|> tabP
      $> CmdHeapLoad

cmdIOP :: TokenParser CmdIO
cmdIOP =
  spaceP
    *> spaceP
    $> CmdIOPrintChar
    <|> spaceP
      *> tabP
      $> CmdIOPrintNum
    <|> tabP
      *> spaceP
      $> CmdIOReadChar
    <|> tabP
      *> tabP
      $> CmdIOReadNum

cmdFlowP :: TokenParser CmdFlow
cmdFlowP =
  spaceP
    *> spaceP
    *> (CmdFlowMark <$> labelP)
    <|> spaceP
      *> tabP
      *> (CmdFlowSub <$> labelP)
    <|> spaceP
      *> lfP
      *> (CmdFlowJump <$> labelP)
    <|> tabP
      *> spaceP
      *> (CmdFlowJumpIfZero <$> labelP)
    <|> tabP
      *> tabP
      *> (CmdFlowJumpIfNeg <$> labelP)
    <|> tabP
      *> lfP
      $> CmdFlowRet
    <|> lfP
      *> lfP
      $> CmdFlowExit
