{-# LANGUAGE OverloadedStrings #-}

module Whitespace.Program
  ( Program (..),
    commandAt,
    commands,
    Command (..),
    CmdStack (..),
    CmdArith (..),
    CmdHeap (..),
    CmdIO (..),
    CmdFlow (..),
    findLabels,
    fromCommands,
    Label,
    parseProgram,
    showProgram,
    validIndex,
  )
where

import Common.Util
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as B
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Vector (Vector)
import Data.Vector qualified as V

-- | A data type representing a Whitespace program.
-- | A program is just a list of commands.
newtype Program = Program {unProgram :: Vector Command} deriving (Eq, Show)

-- | Create a program from a list of commands.
fromCommands :: [Command] -> Program
fromCommands = Program . V.fromList

-- | Get all the commands in the program.
commands :: Program -> [Command]
commands = V.toList . unProgram

-- | Get the command at a specific index in the program.
commandAt :: Program -> Int -> Either String Command
commandAt (Program p) i = p V.!? i |> maybeToEither ("No command at index: " <> show i)

-- | Check if a given index is a valid index in the program.
validIndex :: Program -> Int -> Bool
validIndex (Program p) i = i >= 0 && i < V.length p

-- | Find all the labels in the program.
findLabels :: Program -> Either String (Map Label Int)
findLabels (Program p) = do
  let ls = [(l, i) | (CmdFlow (CmdFlowMark l), i) <- zip (V.toList p) [0 ..]]
  guardE (unique $ fst <$> ls) "Duplicate labels found" (Map.fromList ls)

-- | A data type representing a Whitespace command.
-- | A command can be a stack manipulation command, an arithmetic command, a heap command, an I/O command, or a flow control command.
data Command = CmdStack CmdStack | CmdArith CmdArith | CmdHeap CmdHeap | CmdIO CmdIO | CmdFlow CmdFlow deriving (Eq, Show, Read)

data CmdStack = CmdStackPush Int | CmdStackDup Int | CmdStackDiscard Int | CmdStackDupTop | CmdStackSwap | CmdStackDiscardTop deriving (Eq, Show, Read)

data CmdArith = CmdArithAdd | CmdArithSub | CmdArithMul | CmdArithDiv | CmdArithMod deriving (Eq, Show, Read)

data CmdHeap = CmdHeapStore | CmdHeapLoad deriving (Eq, Show, Read)

data CmdIO = CmdIOPrintChar | CmdIOPrintNum | CmdIOReadChar | CmdIOReadNum deriving (Eq, Show, Read)

data CmdFlow = CmdFlowMark Label | CmdFlowSub Label | CmdFlowJump Label | CmdFlowJumpIfZero Label | CmdFlowJumpIfNeg Label | CmdFlowRet | CmdFlowExit deriving (Eq, Show, Read)

-- | A Token is a character in the Whitespace language. It can be a space, a tab, or a line feed.
-- | A Token is represented as a Char. A space is 's', a tab is 't', and a line feed is 'n'.
type Token = Char

-- | A label in the Whitespace language. A label is just a list of tokens.
type Label = [Token]

showProgram :: Program -> String
showProgram p = unProgram p |> V.toList |>> showCommand |> concat

showCommand :: Command -> String
showCommand (CmdStack c) = " " <> showCmdStack c
showCommand (CmdArith c) = "\t " <> showCmdArith c
showCommand (CmdHeap c) = "\t\t" <> showCmdHeap c
showCommand (CmdIO c) = "\t\n" <> showCmdIO c
showCommand (CmdFlow c) = "\n" <> showCmdFlow c

showInt :: Int -> String
showInt n = sign : showPosInt (abs n) ++ "\n"
  where
    sign = if n < 0 then '\t' else ' '
    showPosInt :: Int -> String
    showPosInt x = toBinReversed x |> reverse |>> digitToToken
    digitToToken :: Int -> Char
    digitToToken 0 = ' '
    digitToToken _ = '\t'
    toBinReversed :: Int -> [Int]
    toBinReversed 0 = []
    toBinReversed x = x `mod` 2 : toBinReversed (x `div` 2)

showLabel :: Label -> String
showLabel l = (l <> "n") |>> showToken
  where
    showToken :: Token -> Char
    showToken 's' = ' '
    showToken 't' = '\t'
    showToken 'n' = '\n'
    showToken _ = error $ "Invalid token in label: " <> show l

showCmdStack :: CmdStack -> String
showCmdStack (CmdStackPush i) = " " <> showInt i
showCmdStack (CmdStackDup i) = "\t " <> showInt i
showCmdStack (CmdStackDiscard i) = "\t\n" <> showInt i
showCmdStack CmdStackDupTop = "\n "
showCmdStack CmdStackSwap = "\n\t"
showCmdStack CmdStackDiscardTop = "\n\n"

showCmdArith :: CmdArith -> String
showCmdArith CmdArithAdd = "  "
showCmdArith CmdArithSub = " \t"
showCmdArith CmdArithMul = " \n"
showCmdArith CmdArithDiv = "\t "
showCmdArith CmdArithMod = "\t\t"

showCmdHeap :: CmdHeap -> String
showCmdHeap CmdHeapStore = " "
showCmdHeap CmdHeapLoad = "\t"

showCmdIO :: CmdIO -> String
showCmdIO CmdIOPrintChar = "  "
showCmdIO CmdIOPrintNum = " \t"
showCmdIO CmdIOReadChar = "\t "
showCmdIO CmdIOReadNum = "\t\t"

showCmdFlow :: CmdFlow -> String
showCmdFlow (CmdFlowMark l) = "  " <> showLabel l
showCmdFlow (CmdFlowSub l) = " \t" <> showLabel l
showCmdFlow (CmdFlowJump l) = " \n" <> showLabel l
showCmdFlow (CmdFlowJumpIfZero l) = "\t " <> showLabel l
showCmdFlow (CmdFlowJumpIfNeg l) = "\t\t" <> showLabel l
showCmdFlow CmdFlowRet = "\t\n"
showCmdFlow CmdFlowExit = "\n\n"

-- | Tokenize a character into a Token.
-- | A space character is tokenized as 's', a tab character is tokenized as 't', and a line feed character is tokenized as 'n'.
-- | All other characters are ignored.
tokeninze :: Char -> Maybe Token
tokeninze ' ' = pure 's'
tokeninze '\t' = pure 't'
tokeninze '\n' = pure 'n'
tokeninze _ = Nothing

-- | Parse a Whitespace program.
parseProgram :: String -> Either String Program
parseProgram = parseOnly programP . B.pack . mapMaybe tokeninze

-- | Parse a complete Whitespace program.
programP :: Parser Program
programP = Program . V.fromList <$> (many commandP <* endOfInput)

-- | Parse a Whitespace command by trying to parse each type of command.
commandP :: Parser Command
commandP =
  (CmdStack <$> ("s" *> cmdStackP))
    <|> (CmdArith <$> ("ts" *> cmdArithP))
    <|> (CmdHeap <$> ("tt" *> cmdHeapP))
    <|> (CmdIO <$> ("tn" *> cmdIOP))
    <|> (CmdFlow <$> ("n" *> cmdFlowP))

-- | Parse a whitespace number.
-- | A number consists of:
-- |   - A sign bit (Space for positive, Tab for negative)
-- |   - A sequence of bits (Space for 0, Tab for 1)
-- |   - A terminating LF
intP :: Parser Int
intP = do
  sign <- ("t" $> -1) <|> ("s" $> 1)
  bits <- many $ ("t" $> 1) <|> ("s" $> 0)
  _ <- "n"
  pure $ convert sign bits 0
  where
    convert s [] acc = s * acc
    convert s (x : xs) acc = convert s xs (acc * 2 + x)

-- | Parse a label. A label is a sequence of tabs and spaces followed by a LF.
labelP :: Parser Label
labelP = manyTill (satisfy (inClass "st")) "n"

-- | Parse a stack manipulation command.
cmdStackP :: Parser CmdStack
cmdStackP =
  ("s" *> (CmdStackPush <$> intP))
    <|> ("ts" *> (CmdStackDup <$> intP))
    <|> ("tn" *> (CmdStackDiscard <$> intP))
    <|> ("ns" $> CmdStackDupTop)
    <|> ("nt" $> CmdStackSwap)
    <|> ("nn" $> CmdStackDiscardTop)

-- | Parse an arithmetic command.
cmdArithP :: Parser CmdArith
cmdArithP =
  ("ss" $> CmdArithAdd)
    <|> ("st" $> CmdArithSub)
    <|> ("sn" $> CmdArithMul)
    <|> ("ts" $> CmdArithDiv)
    <|> ("tt" $> CmdArithMod)

-- | Parse a heap command.
cmdHeapP :: Parser CmdHeap
cmdHeapP =
  ("s" $> CmdHeapStore)
    <|> ("t" $> CmdHeapLoad)

-- | Parse an I/O command.
cmdIOP :: Parser CmdIO
cmdIOP =
  ("ss" $> CmdIOPrintChar)
    <|> ("st" $> CmdIOPrintNum)
    <|> ("ts" $> CmdIOReadChar)
    <|> ("tt" $> CmdIOReadNum)

-- | Parse a flow control command.
cmdFlowP :: Parser CmdFlow
cmdFlowP =
  ("ss" *> (CmdFlowMark <$> labelP))
    <|> ("st" *> (CmdFlowSub <$> labelP))
    <|> ("sn" *> (CmdFlowJump <$> labelP))
    <|> ("ts" *> (CmdFlowJumpIfZero <$> labelP))
    <|> ("tt" *> (CmdFlowJumpIfNeg <$> labelP))
    <|> ("tn" $> CmdFlowRet)
    <|> ("nn" $> CmdFlowExit)
