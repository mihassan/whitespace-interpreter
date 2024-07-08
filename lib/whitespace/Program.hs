{-# LANGUAGE ParallelListComp #-}

module Whitespace.Program
  ( Program,
    commandAt,
    Command (..),
    CmdStack (..),
    CmdArith (..),
    CmdHeap (..),
    CmdIO (..),
    CmdFlow (..),
    findLabels,
    Label,
    programP,
    validIndex,
  )
where

import Common.Util
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Whitespace.Tokenizer (Token)

-- | A data type representing a Whitespace program.
-- | A program is just a list of commands.
newtype Program = Program {unProgram :: Vector Command} deriving (Eq, Show)

-- | A smart constructor for constructing a Whitespace program from a list of commands.
program :: [Command] -> Program
program = Program . V.fromList

-- | Get the command at a specific index in the program.
commandAt :: Program -> Int -> Either String Command
commandAt (Program p) i = p V.!? i |> maybeToEither ("No command at index" <> show i)

-- | Check if a given index is a valid index in the program.
validIndex :: Program -> Int -> Bool
validIndex (Program p) i = i >= 0 && i < V.length p

-- | Find all the labels in the program.
findLabels :: Program -> Either String (Map Label Int)
findLabels (Program p) = do
  let ls = [(l, i) | CmdFlow (CmdFlowMark l) <- V.toList p | i <- [0 ..]]
  guardE (unique $ fst <$> ls) "Duplicate labels found" (Map.fromList ls)

-- | A data type representing a Whitespace command.
-- | A command can be a stack manipulation command, an arithmetic command, a heap command, an I/O command, or a flow control command.
data Command = CmdStack CmdStack | CmdArith CmdArith | CmdHeap CmdHeap | CmdIO CmdIO | CmdFlow CmdFlow deriving (Eq, Show)

data CmdStack = CmdStackPush Int | CmdStackDup Int | CmdStackDiscard Int | CmdStackDupTop | CmdStackSwap | CmdStackDiscardTop deriving (Eq, Show)

data CmdArith = CmdArithAdd | CmdArithSub | CmdArithMul | CmdArithDiv | CmdArithMod deriving (Eq, Show)

data CmdHeap = CmdHeapStore | CmdHeapLoad deriving (Eq, Show)

data CmdIO = CmdIOPrintChar | CmdIOPrintNum | CmdIOReadChar | CmdIOReadNum deriving (Eq, Show)

data CmdFlow = CmdFlowMark Label | CmdFlowSub Label | CmdFlowJump Label | CmdFlowJumpIfZero Label | CmdFlowJumpIfNeg Label | CmdFlowRet | CmdFlowExit deriving (Eq, Show)

-- | A label in the Whitespace language. A label is just a list of tokens.
type Label = [Token]

-- | Parse a Space Token.
spaceP :: Parser Token
spaceP = char 's'

-- | Parse a Tab Token.
tabP :: Parser Token
tabP = char 't'

-- | Parse a LF Token.
lfP :: Parser Token
lfP = char 'n'

-- | Parse a complete Whitespace program.
programP :: Parser Program
programP = program <$> (many commandP <* endOfInput)

-- | Parse a Whitespace command by trying to parse each type of command.
commandP :: Parser Command
commandP =
  (CmdStack <$> (spaceP *> cmdStackP))
    <|> (CmdArith <$> (tabP *> spaceP *> cmdArithP))
    <|> (CmdHeap <$> (tabP *> tabP *> cmdHeapP))
    <|> (CmdIO <$> (tabP *> lfP *> cmdIOP))
    <|> (CmdFlow <$> (lfP *> cmdFlowP))

-- | Parse a whitespace number.
-- | A number consists of:
-- |   - A sign bit (Space for positive, Tab for negative)
-- |   - A sequence of bits (Space for 0, Tab for 1)
-- |   - A terminating LF
intP :: Parser Int
intP = do
  sign <- (tabP $> -1) <|> (spaceP $> 1)
  bits <- many $ (tabP $> 1) <|> (spaceP $> 0)
  _ <- lfP
  pure $ convert sign bits 0
  where
    convert s [] acc = s * acc
    convert s (x : xs) acc = convert s xs (acc * 2 + x)

-- | Parse a label. A label is a sequence of tabs and spaces followed by a LF.
labelP :: Parser Label
labelP = many (tabP <|> spaceP) <* lfP

-- | Parse a stack manipulation command.
cmdStackP :: Parser CmdStack
cmdStackP =
  (spaceP *> (CmdStackPush <$> intP))
    <|> (tabP *> spaceP *> (CmdStackDup <$> intP))
    <|> (tabP *> lfP *> (CmdStackDiscard <$> intP))
    <|> (lfP *> spaceP $> CmdStackDupTop)
    <|> (lfP *> tabP $> CmdStackSwap)
    <|> (lfP *> lfP $> CmdStackDiscardTop)

-- | Parse an arithmetic command.
cmdArithP :: Parser CmdArith
cmdArithP =
  (spaceP *> spaceP $> CmdArithAdd)
    <|> (spaceP *> tabP $> CmdArithSub)
    <|> (spaceP *> lfP $> CmdArithMul)
    <|> (tabP *> spaceP $> CmdArithDiv)
    <|> (tabP *> tabP $> CmdArithMod)

-- | Parse a heap command.
cmdHeapP :: Parser CmdHeap
cmdHeapP =
  (spaceP $> CmdHeapStore)
    <|> (tabP $> CmdHeapLoad)

-- | Parse an I/O command.
cmdIOP :: Parser CmdIO
cmdIOP =
  (spaceP *> spaceP $> CmdIOPrintChar)
    <|> (spaceP *> tabP $> CmdIOPrintNum)
    <|> (tabP *> spaceP $> CmdIOReadChar)
    <|> (tabP *> tabP $> CmdIOReadNum)

-- | Parse a flow control command.
cmdFlowP :: Parser CmdFlow
cmdFlowP =
  (spaceP *> spaceP *> (CmdFlowMark <$> labelP))
    <|> (spaceP *> tabP *> (CmdFlowSub <$> labelP))
    <|> (spaceP *> lfP *> (CmdFlowJump <$> labelP))
    <|> (tabP *> spaceP *> (CmdFlowJumpIfZero <$> labelP))
    <|> (tabP *> tabP *> (CmdFlowJumpIfNeg <$> labelP))
    <|> (tabP *> lfP $> CmdFlowRet)
    <|> (lfP *> lfP $> CmdFlowExit)
