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
        \s -> [(b,s'') | (a,s') <- p s, (b,s'') <- parse (f a) s']
)

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


-- So, for example, if my type can be compared for equality (most types can, but some, particularly function types, cannot) then I can give an instance declaration of the Eq class. All I have to do is specify the behaviour of the == operator on my type, and I gain the ability to use all sorts of functions defined using that operator, e.g. checking if a value of my type is present in a list, or looking up a corresponding value in a list of pairs.


------------------------------------------------------------------------------

failToParse :: Parser a
failToParse = MkParser $ \s -> []

option :: Parser a -> Parser a -> Parser a
option (MkParser p) (MkParser q) = MkParser (\x -> case p x of
                                                        [] ->
                                                        res -> )

instance Alternative Parser where
  empty = failToParse
  (<|>) = option

satisfy :: (Char -> Bool) -> Parser Char
satisfy p (MkParser f) = do
        c <- item
        if p c
          then return c
          else empty

anyOf :: [Char] -> Parser Char
anyOf cs = satisfy (\c -> c `elem cs`)
-- anyOf cs = do
  --      c <- item
    --    if c `elem` cs
      --    then return c
        --  else empty

data EMail = MkEMail {getAddr :: String, getDomain :: String, getExtension :: String} deriving Show

-- myaddr@domain.(com|edu|org)
parseEmail :: Parser EMail
parseEMail = do
      addr <- some (satisfy (/= "@"))
      _ <- satisfy (== '@')
      domName <- some (satisfy (/= '.'))
      extn <- some item
      return $ MkEmail addr domName extn


-- Lipton Lecture

-- A topological space is a set X and a family T of subsets of X Satisfying
-- -T is closed under union
-- -closed under finite intersection
-- -the empty set is not in T
-- -x is in T

-- MONADS
-- Let F be a Functor from a _Cagetory_ C to itself F:C->C
-- F is a "closure operator" if for every object A of C there is an arrow
-- A --r_A--> F(A)
-- A --j_A--> F(F(A))

-- in haskell:
-- a --return--> ma
-- m(ma) --join--> ma
