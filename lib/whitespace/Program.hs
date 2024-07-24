{-# LANGUAGE OverloadedStrings #-}

module Whitespace.Program
  ( Program (..),
    Instruction (..),
    Label,
    findLabels,
    fromInstructions,
    instructions,
    instructionAt,
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
-- | A program is just a list of instructions.
newtype Program = Program {unProgram :: Vector Instruction} deriving (Eq, Show)

-- | A Token is a character in the Whitespace language. It can be a space, a tab, or a line feed.
-- | A Token is represented as a Char. A space is 's', a tab is 't', and a line feed is 'n'.
type Token = Char

-- | A label in the Whitespace language. A label is just a list of tokens.
type Label = [Token]

-- | A data type representing a single Whitespace instruction.
data Instruction
  = PUSH Int
  | DUP Int
  | DISCARD Int
  | DUP_TOP
  | SWAP
  | DISCARD_TOP
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | STORE
  | LOAD
  | WRITE_CHAR
  | WRITE_NUM
  | READ_CHAR
  | READ_NUM
  | MARK Label
  | CALL Label
  | JUMP Label
  | JZ Label
  | JNG Label
  | RET
  | EXIT
  deriving (Eq, Show, Read)

-- | Create a program from a list of instructions.
fromInstructions :: [Instruction] -> Program
fromInstructions = Program . V.fromList

-- | Get the instructions in the program.
instructions :: Program -> [Instruction]
instructions = V.toList . unProgram

-- | Get the instruction at a specific index in the program.
instructionAt :: Program -> Int -> Either String Instruction
instructionAt (Program p) i = p V.!? i |> maybeToEither ("No instruction at index: " <> show i)

-- | Check if a given index is a valid index in the program.
validIndex :: Program -> Int -> Bool
validIndex (Program p) i = i >= 0 && i < V.length p

-- | Find all the labels in the program.
findLabels :: Program -> Either String (Map Label Int)
findLabels (Program p) = do
  let ls = [(l, i) | (MARK l, i) <- zip (V.toList p) [0 ..]]
  guardE (unique $ fst <$> ls) "Duplicate labels found" (Map.fromList ls)

showProgram :: Program -> String
showProgram p = unProgram p |> V.toList |>> showInstruction |> concat

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

showInstruction :: Instruction -> String
showInstruction = \case
  (PUSH i) -> "  " <> showInt i
  (DUP i) -> " \t " <> showInt i
  (DISCARD i) -> " \t\n" <> showInt i
  DUP_TOP -> " \n "
  SWAP -> " \n\t"
  DISCARD_TOP -> " \n\n"
  ADD -> "\t   "
  SUB -> "\t  \t"
  MUL -> "\t  \n"
  DIV -> "\t \t "
  MOD -> "\t \t\t"
  STORE -> "\t\t "
  LOAD -> "\t\t\t"
  WRITE_CHAR -> "\t\n  "
  WRITE_NUM -> "\t\n \t"
  READ_CHAR -> "\t\n\t "
  READ_NUM -> "\t\n\t\t"
  (MARK l) -> "\n  " <> showLabel l
  (CALL l) -> "\n \t" <> showLabel l
  (JUMP l) -> "\n \n" <> showLabel l
  (JZ l) -> "\n\t " <> showLabel l
  (JNG l) -> "\n\t\t" <> showLabel l
  RET -> "\n\t\n"
  EXIT -> "\n\n\n"

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
programP = Program . V.fromList <$> (many instructionP <* endOfInput)

-- | Parse a Whitespace instruction.
instructionP :: Parser Instruction
instructionP =
  ("s" *> stackP)
    <|> ("ts" *> arithP)
    <|> ("tt" *> heapP)
    <|> ("tn" *> ioP)
    <|> ("n" *> flowP)

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

-- | Parse a stack manipulation instruction.
stackP :: Parser Instruction
stackP =
  ("s" *> (PUSH <$> intP))
    <|> ("ts" *> (DUP <$> intP))
    <|> ("tn" *> (DISCARD <$> intP))
    <|> ("ns" $> DUP_TOP)
    <|> ("nt" $> SWAP)
    <|> ("nn" $> DISCARD_TOP)

-- | Parse an arithmetic instruction.
arithP :: Parser Instruction
arithP =
  ("ss" $> ADD)
    <|> ("st" $> SUB)
    <|> ("sn" $> MUL)
    <|> ("ts" $> DIV)
    <|> ("tt" $> MOD)

-- | Parse a heap instruction.
heapP :: Parser Instruction
heapP =
  ("s" $> STORE)
    <|> ("t" $> LOAD)

-- | Parse an I/O instruction.
ioP :: Parser Instruction
ioP =
  ("ss" $> WRITE_CHAR)
    <|> ("st" $> WRITE_NUM)
    <|> ("ts" $> READ_CHAR)
    <|> ("tt" $> READ_NUM)

-- | Parse a flow control instruction.
flowP :: Parser Instruction
flowP =
  ("ss" *> (MARK <$> labelP))
    <|> ("st" *> (CALL <$> labelP))
    <|> ("sn" *> (JUMP <$> labelP))
    <|> ("ts" *> (JZ <$> labelP))
    <|> ("tt" *> (JNG <$> labelP))
    <|> ("tn" $> RET)
    <|> ("nn" $> EXIT)
