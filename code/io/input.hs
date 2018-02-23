import System.IO
import Text.PrettyPrint.Boxes

main :: IO ()
main = do
    handle <- openFile "transactions.csv" ReadMode
    contents <- hGetContents handle
    let sepContents = lines contents
    putStr toTable sepContents
    hClose handle


parse :: [String] -> [[String]]
parse ls = [ls]

toTable :: [[String]] -> String
toTable ls = "http://hackage.haskell.org/package/boxes-0.1.4/docs/Text-PrettyPrint-Boxes.html"
