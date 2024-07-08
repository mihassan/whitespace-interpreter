module Whitespace.Parser (Parser (..), parse, satisfy, eof) where

import Common.Util
import Control.Applicative
import Control.Monad

-- | A simple parser type. The parser is a function that takes an input and returns
-- | a pair of the remaining input and the parsed value.
-- | The Parser is defined to be instance of Functor, Applicative, Monad, and Alternative.
-- | This allows us to use the do-notation and combinators like (<|>) and many more.
data Parser s a = Parser
  { runParser :: s -> Either String (s, a)
  }
  deriving (Functor)

instance Applicative (Parser s) where
  pure a = Parser $ \s -> pure (s, a)
  (<*>) = ap

instance Monad (Parser s) where
  return = pure
  p >>= f = Parser $ \s -> do
    (s', a) <- runParser p s
    runParser (f a) s'

instance Alternative (Parser s) where
  empty = Parser . const $ Left "empty"
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Left _ -> runParser p2 s
    r -> r

-- | Parse the given input with the given parser.
-- | Returns Just the parsed value if the parser succeeded, otherwise Nothing.
-- | The parser is considered to have succeeded if the input is fully consumed.
parse :: Parser s a -> s -> Either String a
parse p s = runParser p s |>> snd

-- | Parse a value that satisfies the given predicate.
-- | This can be used to define more complex parsers.
satisfy :: (a -> Bool) -> Parser [a] a
satisfy p = Parser $ \s -> case s of
  t : ts | p t -> pure (ts, t)
  _ -> Left "satisfy"

-- | Parse the end of the input.
eof :: Parser [a] ()
eof = Parser $ \s -> case s of
  [] -> pure ([], ())
  _ -> Left "eof"
