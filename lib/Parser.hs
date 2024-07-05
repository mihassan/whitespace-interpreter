module Parser (Parser (..), parse, token, satisfy, eof) where

import Control.Applicative
import Control.Monad

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

parse :: Parser s a -> s -> Maybe a
parse p s = case runParser p s of
  Just (_, a) -> Just a
  _ -> Nothing

token :: Parser [a] a
token = Parser $ \s -> case s of
  t : ts -> Just (ts, t)
  _ -> Nothing

satisfy :: (a -> Bool) -> Parser [a] a
satisfy p = token >>= \t -> if p t then pure t else empty

eof :: Parser [a] ()
eof = (token *> empty) <|> pure ()
