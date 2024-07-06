module Whitespace.Parser (Parser (..), parse, satisfy, eof) where

import Control.Applicative
import Control.Monad

-- | A simple parser type. The parser is a function that takes an input and returns
-- | a pair of the remaining input and the parsed value.
-- | The Parser is defined to be instance of Functor, Applicative, Monad, and Alternative.
-- | This allows us to use the do-notation and combinators like (<|>) and many more.
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

-- | Parse the given input with the given parser.
-- | Returns Just the parsed value if the parser succeeded, otherwise Nothing.
-- | The parser is considered to have succeeded if the input is fully consumed.
parse :: Parser s a -> s -> Maybe a
parse p s = case runParser p s of
  Just (_, a) -> Just a
  _ -> Nothing

-- | Parse a value that satisfies the given predicate.
-- | This can be used to define more complex parsers.
satisfy :: (a -> Bool) -> Parser [a] a
satisfy p = Parser $ \s -> case s of
  t : ts | p t -> Just (ts, t)
  _ -> Nothing

-- | Parse the end of the input.
eof :: Parser [a] ()
eof = Parser $ \s -> case s of
  [] -> Just ([], ())
  _ -> Nothing
