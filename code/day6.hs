-- Lipton Lecture

-- _Group_ (G, *, e)
-- G a set
-- (*) G x G -> G
--  (*) (g1, g2) = g1 * g2
-- There is an element e in G (the unit)
-- s.t. - g * e = e * g = g
--  g1(g2g3) = (g1g2)g3
--  For every g there is a unique g^-1 s.t. g*g^-1 = e

-- _Monoid_ (M, *, 1)
-- unit m*1 = 1*m = m
-- associative
-- no inverse required

-- _Category_ C is a collection of objects and arrows
-- -------v target
-- A -f-> B
-- ^ source
-- Satisfying: For each object A there is an arrow idA: A -> A
-- There is a partial operation called Composition of arrows, defined for
-- A -f-> B and B -g-> C which yields A -(g o f)-> C
-- g o (f o h) = (g o f) o h
-- A -idA-> A -f-> B = A -f-> B = A -f-> B -idB-> B
-- ex. (Sets, functions) (Groups, homomorphisms) (Vector spaces, linear tranformations) (Types, programs)


import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List

first :: (a -> b) -> (a,c) -> (b, c)
first f (a, c) = (f a, c)

-- a parser of strings is a function from strings to lists of things and strings

data Parser a = MkParser {parse :: String -> [(a,String)]}

mapParser :: (a -> b) -> (Parser a) -> (Parser b)
mapParser f (MkParser p) = MkParser (
        \s -> fmap (\(a,s') -> (f a,s')) (p s))

instance Functor Parser where
  fmap = mapParser

unitParse :: a -> Parser a
unitParse x = MkParser (\s -> [(x,s)])

bindParse :: Parser a -> (a -> Parser b) -> Parser b
bindParse (MkParser p) f = MkParser (
        \s -> [(b,s'') | (a,s') <- p s, (b,s'') <- parse (f a) s'])

instance Monad Parser where
  (>==) = bindParse
  return = unitParse

instance Applicative Parser where
  (<*>) = ap
  pure = return

item :: Parser Char
item = MkParser (\s -> case s of
                        "" -> []
                        (a:as) -> [(a,as)])

parse3 :: Parser String
parse3 = do
        a <- item
        b <- item
        c <- item
        pure (a:b:c:[])

parse6 :: Parser String
parse6 = do
        first3 <- parse3
        second3 <- parse3
        return (first3 ++ second3)
