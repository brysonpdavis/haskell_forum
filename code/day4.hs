module Lec4 where
import Control.Monad

prices :: [(String, Int)]
prices = [("milk", 3), ("bread", 2), ("yogurt", 5)]

lookupE :: (Show a, Eq a) => a -> [(a, b)] -> Either String b
lookupE x [] = Left $ "Couldn't find " ++ show x
lookupE x ((y, z) : ys) =
  if x == y then Right z else lookupE x ys

milkAndYogurt :: Maybe Int
milkAndYogurt =
  case lookup "milk" prices of
    Nothing -> Nothing
    Just milk -> case lookup "yogurt" prices of
      Nothing -> Nothing
      Just yogurt -> Just $ milk + yogurt

milkAndYogurt' :: Either String Int
milkAndYogurt' =
  (lookupE "milk" prices) >>= (\milk ->
    ((lookupE "yogurt" prices) >>= (\yogurt ->
      pure (milk + yogurt))))

milkAndYogurt'' :: Either String Int
milkAndYogurt'' = do
  milk <- lookupE "milk" prices
  yogurt <- lookupE "yogurt" prices
  pure (milk + yogurt)

milkAndYogurt''' :: Either String Int
milkAndYogurt''' =
  (+) <$> (lookupE "milk" prices) <*> (lookupE "yogurt" prices)

sumTwo :: String -> String -> Either String Int
sumTwo x y = do
  x' <- lookupE x prices
  y' <- lookupE y prices
  pure (x' + y')

-----

data Option a = Some a | None

instance Monad Option where
  return = Some
  x >>= f = case x of
    Some x' -> f x'
    None -> None

instance Applicative Option where
  pure = return
  (<*>) = ap

instance Functor Option where
  fmap = liftM

---------

data State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Functor (State s) where
  fmap = liftM

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put x = State $ \_ -> ((),x)
