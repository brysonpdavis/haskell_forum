module Game where

import System.Random

data Move = Rock | Paper | Scissors
  deriving (Show, Eq, Enum)

instance Ord Move where
  x <= y = x == y ||
          (x, y) `elem` [ (Rock, Paper)
                        , (Paper, Scissors)
                        , (Scissors, Rock) ]

humanSelect :: IO Move
humanSelect = fmap (toEnum . read) getLine

{-
humanSelect = do
  x <- getLine
  pure ((toEnum . read))
  -}

computerSelect :: IO Move
computerSelect = fmap toEnum (randomRIO (0,2))

main :: IO () -- () means unit
main = do
  putStrLn "Choose a move: 0 for Rock, 1 for Paper, 2 for Scissors"
  x <- humanSelect
  y <- computerSelect
  putStrLn ("Computer selected " ++ show y)
  case compare x y of
    GT -> putStrLn "Player wins!"
    EQ -> putStrLn "Draw!"
    LT -> putStrLn "Computer wins!"
  main
