isEven :: Int -> Bool
isEven x = (mod x 2) == 0

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

-- finds distance from origin to a point
norm :: (Float, Float) -> Float
norm (x, y) = sqrt (x*x + y*y)

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = (f x) : (mapInt f xs)
