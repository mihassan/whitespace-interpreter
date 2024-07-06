module Whitespace.Tokenizer (Token (..), tokenize) where

data Token = Space | Tab | LF deriving (Eq, Ord, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ' : xs) = Space : tokenize xs
tokenize ('\t' : xs) = Tab : tokenize xs
tokenize ('\n' : xs) = LF : tokenize xs
tokenize (_ : xs) = tokenize xs
