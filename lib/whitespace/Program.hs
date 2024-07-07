module Whitespace.Program
  ( Program,
    commandAt,
    commands,
    Command (..),
    CmdStack (..),
    CmdArith (..),
    CmdHeap (..),
    CmdIO (..),
    CmdFlow (..),
    Number,
    Label,
    programP,
    validIndex,
  )
where

import Control.Applicative
import Data.Functor
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Whitespace.Parser
import Whitespace.Tokenizer (Token (..))

-- | A data type representing a Whitespace program.
-- | A program is just a list of commands.
newtype Program = Program {unProgram :: IntMap Command} deriving (Eq, Show)

-- | A smart constructor for constructing a Whitespace program from a list of commands.
program :: [Command] -> Program
program = Program . IntMap.fromAscList . zip [0 ..]

commands :: Program -> [Command]
commands = IntMap.elems . unProgram

-- | Get the command at a specific index in the program.
commandAt :: Program -> Int -> Maybe Command
commandAt (Program p) i = IntMap.lookup i p

-- | Check if a given index is a valid index in the program.
validIndex :: Program -> Int -> Bool
validIndex (Program p) i = IntMap.member i p

-- | A data type representing a Whitespace command.
-- | A command can be a stack manipulation command, an arithmetic command, a heap command, an I/O command, or a flow control command.
data Command = CmdStack CmdStack | CmdArith CmdArith | CmdHeap CmdHeap | CmdIO CmdIO | CmdFlow CmdFlow deriving (Eq, Show)

data CmdStack = CmdStackPush Number | CmdStackDup Number | CmdStackDiscard Number | CmdStackDupTop | CmdStackSwap | CmdStackDiscardTop deriving (Eq, Show)

data CmdArith = CmdArithAdd | CmdArithSub | CmdArithMul | CmdArithDiv | CmdArithMod deriving (Eq, Show)

data CmdHeap = CmdHeapStore | CmdHeapLoad deriving (Eq, Show)

data CmdIO = CmdIOPrintChar | CmdIOPrintNum | CmdIOReadChar | CmdIOReadNum deriving (Eq, Show)

data CmdFlow = CmdFlowMark Label | CmdFlowSub Label | CmdFlowJump Label | CmdFlowJumpIfZero Label | CmdFlowJumpIfNeg Label | CmdFlowRet | CmdFlowExit deriving (Eq, Show)

-- | A number in the Whitespace language. A number is just an integer.
type Number = Int

-- | A label in the Whitespace language. A label is just a list of tokens.
type Label = [Token]

-- | A TokenParser is a parser that parses a list of tokens.
type TokenParser a = Parser [Token] a

-- | Parse any Token.
tokenP :: Token -> TokenParser Token
tokenP t = satisfy (== t)

-- | Parse a Space Token.
spaceP :: TokenParser Token
spaceP = tokenP Space

-- | Parse a Tab Token.
tabP :: TokenParser Token
tabP = tokenP Tab

-- | Parse a LF Token.
lfP :: TokenParser Token
lfP = tokenP LF

-- | Parse a complete Whitespace program.
programP :: TokenParser Program
programP = program <$> (many commandP <* eof)

-- | Parse a Whitespace command by trying to parse each type of command.
commandP :: TokenParser Command
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
numberP :: TokenParser Number
numberP = do
  sign <- (tabP $> -1) <|> (spaceP $> 1)
  bits <- many $ (tabP $> 1) <|> (spaceP $> 0)
  _ <- lfP
  pure $ convert sign bits 0
  where
    convert s [] acc = s * acc
    convert s (x : xs) acc = convert s xs (acc * 2 + x)

-- | Parse a label. A label is a sequence of tabs and spaces followed by a LF.
labelP :: TokenParser Label
labelP = many (tabP <|> spaceP) <* lfP

-- | Parse a stack manipulation command.
cmdStackP :: TokenParser CmdStack
cmdStackP =
  (spaceP *> (CmdStackPush <$> numberP))
    <|> (tabP *> spaceP *> (CmdStackDup <$> numberP))
    <|> (tabP *> lfP *> (CmdStackDiscard <$> numberP))
    <|> (lfP *> spaceP $> CmdStackDupTop)
    <|> (lfP *> tabP $> CmdStackSwap)
    <|> (lfP *> lfP $> CmdStackDiscardTop)

-- | Parse an arithmetic command.
cmdArithP :: TokenParser CmdArith
cmdArithP =
  (spaceP *> spaceP $> CmdArithAdd)
    <|> (spaceP *> tabP $> CmdArithSub)
    <|> (spaceP *> lfP $> CmdArithMul)
    <|> (tabP *> spaceP $> CmdArithDiv)
    <|> (tabP *> tabP $> CmdArithMod)

-- | Parse a heap command.
cmdHeapP :: TokenParser CmdHeap
cmdHeapP =
  (spaceP $> CmdHeapStore)
    <|> (tabP $> CmdHeapLoad)

-- | Parse an I/O command.
cmdIOP :: TokenParser CmdIO
cmdIOP =
  (spaceP *> spaceP $> CmdIOPrintChar)
    <|> (spaceP *> tabP $> CmdIOPrintNum)
    <|> (tabP *> spaceP $> CmdIOReadChar)
    <|> (tabP *> tabP $> CmdIOReadNum)

-- | Parse a flow control command.
cmdFlowP :: TokenParser CmdFlow
cmdFlowP =
  (spaceP *> spaceP *> (CmdFlowMark <$> labelP))
    <|> (spaceP *> tabP *> (CmdFlowSub <$> labelP))
    <|> (spaceP *> lfP *> (CmdFlowJump <$> labelP))
    <|> (tabP *> spaceP *> (CmdFlowJumpIfZero <$> labelP))
    <|> (tabP *> tabP *> (CmdFlowJumpIfNeg <$> labelP))
    <|> (tabP *> lfP $> CmdFlowRet)
    <|> (lfP *> lfP $> CmdFlowExit)
