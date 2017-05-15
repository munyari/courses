{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

import           Control.Monad (void)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,b) = (f a, b)

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p1) = Parser p2
        where p2 s = fmap (first f) (p1 s)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure val = Parser (\s -> Just (val, s))

    -- p1 :: String -> Maybe (a -> b, String)
    -- p2 :: String -> Maybe (a, String)
    -- first :: (a -> b) -> (a, c) -> (b, c)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser res
       where res s = case p1 s of
                       Nothing -> Nothing
                       Just (f, s1) -> case p2 s1 of
                                        Nothing -> Nothing
                                        Just (a1, s2) -> Just (f a1, s2)

abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ z -> x:[z]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p1) <|> (Parser p2) = Parser p3
                                    where p3 s = case p1 s of
                                                   Nothing -> p2 s
                                                   null -> p1 s

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
