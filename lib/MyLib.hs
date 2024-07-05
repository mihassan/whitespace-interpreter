{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module MyLib (whitespace) where

import Control.Applicative
import Control.Monad
import Data.Char (chr, ord)
import Data.Functor
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Read
import Util

type ErrorMsg = String

type Result = Either ErrorMsg String

-- Tokenizer

data Token = Space | Tab | LF deriving (Eq, Ord, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ' : xs) = Space : tokenize xs
tokenize ('\t' : xs) = Tab : tokenize xs
tokenize ('\n' : xs) = LF : tokenize xs
tokenize (_ : xs) = tokenize xs

-- Parser

data Parser s a = Parser
  { runParser :: s -> Maybe (s, a)
  }
  deriving (Functor)

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (s, a)
  (<*>) = ap

instance Monad (Parser s) where
  return = pure
  p >>= f = Parser $ \s -> do
    (s', a) <- runParser p s
    runParser (f a) s'

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

type TokenParser a = Parser [Token] a

type Number = Int

type Label = [Token]

type Program = [Command]

data Command = CmdStack CmdStack | CmdArith CmdArith | CmdHeap CmdHeap | CmdIO CmdIO | CmdFlow CmdFlow deriving (Eq, Show)

data CmdStack = CmdStackPush Number | CmdStackDup Number | CmdStackDiscard Number | CmdStackDupTop | CmdStackSwap | CmdStackDiscardTop deriving (Eq, Show)

data CmdArith = CmdArithAdd | CmdArithSub | CmdArithMul | CmdArithDiv | CmdArithMod deriving (Eq, Show)

data CmdHeap = CmdHeapStore | CmdHeapLoad deriving (Eq, Show)

data CmdIO = CmdIOPrintChar | CmdIOPrintNum | CmdIOReadChar | CmdIOReadNum deriving (Eq, Show)

data CmdFlow = CmdFlowMark Label | CmdFlowSub Label | CmdFlowJump Label | CmdFlowJumpIfZero Label | CmdFlowJumpIfNeg Label | CmdFlowRet | CmdFlowExit deriving (Eq, Show)

parse :: TokenParser a -> [Token] -> Maybe a
parse p s = case runParser p s of
  Just (_, a) -> Just a
  _ -> Nothing

anyToken :: TokenParser Token
anyToken = Parser $ \s -> case s of
  t : ts -> Just (ts, t)
  _ -> Nothing

satisfy :: (Token -> Bool) -> TokenParser Token
satisfy p = anyToken >>= \t -> if p t then pure t else empty

eof :: TokenParser ()
eof = (anyToken *> empty) <|> pure ()

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
  CmdStack <$> (spaceP *> cmdStackP)
    <|> CmdArith <$> (tabP *> spaceP *> cmdArithP)
    <|> CmdHeap <$> (tabP *> tabP *> cmdHeapP)
    <|> CmdIO <$> (tabP *> lfP *> cmdIOP)
    <|> CmdFlow <$> (lfP *> cmdFlowP)

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
  spaceP *> (CmdStackPush <$> numberP)
    <|> tabP *> spaceP *> (CmdStackDup <$> numberP)
    <|> tabP *> lfP *> (CmdStackDiscard <$> numberP)
    <|> lfP *> spaceP $> CmdStackDupTop
    <|> lfP *> tabP $> CmdStackSwap
    <|> lfP *> lfP $> CmdStackDiscardTop

cmdArithP :: TokenParser CmdArith
cmdArithP =
  spaceP *> spaceP $> CmdArithAdd
    <|> spaceP *> tabP $> CmdArithSub
    <|> spaceP *> lfP $> CmdArithMul
    <|> tabP *> spaceP $> CmdArithDiv
    <|> tabP *> tabP $> CmdArithMod

cmdHeapP :: TokenParser CmdHeap
cmdHeapP =
  spaceP $> CmdHeapStore
    <|> tabP $> CmdHeapLoad

cmdIOP :: TokenParser CmdIO
cmdIOP =
  spaceP *> spaceP $> CmdIOPrintChar
    <|> spaceP *> tabP $> CmdIOPrintNum
    <|> tabP *> spaceP $> CmdIOReadChar
    <|> tabP *> tabP $> CmdIOReadNum

cmdFlowP :: TokenParser CmdFlow
cmdFlowP =
  spaceP *> spaceP *> (CmdFlowMark <$> labelP)
    <|> spaceP *> tabP *> (CmdFlowSub <$> labelP)
    <|> spaceP *> lfP *> (CmdFlowJump <$> labelP)
    <|> tabP *> spaceP *> (CmdFlowJumpIfZero <$> labelP)
    <|> tabP *> tabP *> (CmdFlowJumpIfNeg <$> labelP)
    <|> tabP *> lfP $> CmdFlowRet
    <|> lfP *> lfP $> CmdFlowExit

-- Interpreter

data ProcessStatus = Running | Exit deriving (Eq, Show)

data Process = Process
  { program :: Program,
    input :: String,
    output :: String,
    stack :: [Number],
    heap :: Map Number Number,
    labels :: Map Label Int,
    callStack :: [Int],
    ip :: Int,
    status :: ProcessStatus
  }
  deriving (Eq, Show)

mkProcess :: String -> Program -> Process
mkProcess input program = Process {output = "", stack = [], heap = Map.empty, labels = Map.empty, callStack = [], ip = 0, status = Running, ..}

markLabels :: Process -> Maybe Process
markLabels Process {..} = do
  let ls = [(l, i) | (CmdFlow (CmdFlowMark l), i) <- zip program [0 ..]]
  guard . not . hasDuplicates $ fst <$> ls
  pure $ Process {labels = Map.fromList ls, ..}

runProcess :: Process -> Maybe Process
runProcess p@(Process {status = Exit, ..}) = Just p
runProcess p@(Process {..}) = do
  c <- nextCommand program ip
  p' <- runCommand c p
  runProcess p'

nextCommand :: Program -> Int -> Maybe Command
nextCommand p i = p !? i

push :: Process -> Number -> Process
push Process {..} n = Process {stack = n : stack, ..}

discard :: Process -> Number -> Process
discard Process {..} n = case stack of
  (x : xs) -> Process {stack = x : drop n' xs, ..}
  otehrwise -> Process {..}
  where
    n' = if n >= 0 then n else length stack

readInt :: Process -> Maybe (Int, Process)
readInt Process {..} = do
  guard . not . null $ input
  let (x : xs) = lines input
  i <- readMaybe x
  pure (i, Process {input = unlines xs, ..})

readChar :: Process -> Maybe (Char, Process)
readChar Process {..} = do
  guard . not . null $ input
  let (x : xs) = input
  pure (x, Process {input = xs, ..})

incIp :: Process -> Process
incIp p = p {ip = ip p + 1}

runCommand :: Command -> Process -> Maybe Process
runCommand (CmdStack c) = fmap incIp <$> runCmdStack c
runCommand (CmdArith c) = fmap incIp <$> runCmdArith c
runCommand (CmdHeap c) = fmap incIp <$> runCmdHeap c
runCommand (CmdIO c) = fmap incIp <$> runCmdIO c
runCommand (CmdFlow c) = runCmdFlow c

runCmdStack :: CmdStack -> Process -> Maybe Process
runCmdStack (CmdStackPush n) p = Just $ push p n
runCmdStack (CmdStackDup n) p@(Process {..}) = stack !? n |>> push p
runCmdStack (CmdStackDiscard n) p@(Process {..})
  | null stack = Nothing
  | otherwise = Just $ discard p n
runCmdStack CmdStackDupTop Process {..} = case stack of
  (x : xs) -> Just Process {stack = x : x : xs, ..}
  otherwise -> Nothing
runCmdStack CmdStackSwap Process {..} = case stack of
  (x : y : ys) -> Just Process {stack = y : x : ys, ..}
  otherwise -> Nothing
runCmdStack CmdStackDiscardTop Process {..}
  | null stack = Nothing
  | otherwise = Just Process {stack = tail stack, ..}

arithOp :: CmdArith -> Number -> Number -> Maybe Number
arithOp CmdArithAdd a b = Just $ b + a
arithOp CmdArithSub a b = Just $ b - a
arithOp CmdArithMul a b = Just $ b * a
arithOp CmdArithDiv 0 b = Nothing
arithOp CmdArithDiv a b = Just $ b `div` a
arithOp CmdArithMod 0 b = Nothing
arithOp CmdArithMod a b = Just $ b `mod` a

runCmdArith :: CmdArith -> Process -> Maybe Process
runCmdArith c Process {..} = case stack of
  (a : b : rs) -> arithOp c a b |>> \x -> Process {stack = x : rs, ..}
  otherwise -> Nothing

runCmdHeap :: CmdHeap -> Process -> Maybe Process
runCmdHeap CmdHeapStore Process {..} = case stack of
  (a : b : rs) -> Just $ Process {stack = rs, heap = Map.insert b a heap, ..}
  otherwise -> Nothing
runCmdHeap CmdHeapLoad Process {..} = case stack of
  (a : rs) -> Map.lookup a heap |>> \x -> Process {stack = x : rs, ..}
  otherwise -> Nothing

runCmdIO :: CmdIO -> Process -> Maybe Process
runCmdIO CmdIOPrintChar p@(Process {..}) = case stack of
  (x : xs) -> Just Process {output = output ++ [chr x], stack = xs, ..}
  otherwise -> Nothing
runCmdIO CmdIOPrintNum p@(Process {..}) = case stack of
  (x : xs) -> Just Process {output = output ++ show x, stack = xs, ..}
  otherwise -> Nothing
runCmdIO CmdIOReadChar p@(Process {..}) = case stack of
  (b : rs) -> do
    (c, p') <- readChar p
    let a = ord c
    pure p' {heap = Map.insert b a heap, stack = rs}
  otherwise -> Nothing
runCmdIO CmdIOReadNum p@(Process {..}) = case stack of
  (b : rs) -> do
    (a, p') <- readInt p
    pure p' {heap = Map.insert b a heap, stack = rs}
  otherwise -> Nothing

runCmdFlow :: CmdFlow -> Process -> Maybe Process
runCmdFlow (CmdFlowMark l) p@(Process {..}) = incIp <$> Just Process {labels = Map.insert l ip labels, ..}
runCmdFlow (CmdFlowSub l) p@(Process {..}) = Map.lookup l labels >>= \x -> Just Process {callStack = (ip + 1) : callStack, ip = x, ..}
runCmdFlow (CmdFlowJump l) p@(Process {..}) = Map.lookup l labels >>= \x -> Just Process {ip = x, ..}
runCmdFlow (CmdFlowJumpIfZero l) p@(Process {..}) = case stack of
  (x : xs) | x == 0 -> Map.lookup l labels >>= \ip' -> Just Process {ip = ip', stack = xs, ..}
  (x : xs) | otherwise -> incIp <$> Just Process {stack = xs, ..}
  otherwise -> Nothing
runCmdFlow (CmdFlowJumpIfNeg l) p@(Process {..}) = case stack of
  (x : xs) | x < 0 -> Map.lookup l labels >>= \ip' -> Just Process {ip = ip', stack = xs, ..}
  (x : xs) | otherwise -> incIp <$> Just Process {stack = xs, ..}
  otherwise -> Nothing
runCmdFlow CmdFlowRet p@(Process {..}) = case callStack of
  (x : xs) -> Just Process {ip = x, callStack = xs, ..}
  otherwise -> Nothing
runCmdFlow CmdFlowExit p@(Process {..}) = Just Process {status = Exit, ..}

getResult :: Maybe Process -> Result
getResult Nothing = Left ""
getResult (Just p) = Right $ output p

-- Main solution

whitespace :: String -> String -> Result
whitespace code input = tokenize code |> parse programP |>> mkProcess input >>= markLabels >>= runProcess |> getResult
