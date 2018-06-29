{-
==============================================
Davis-Rahman Basic Linear Algebra Library
==============================================
COMP 420 | Functional Programming in Haskell
Wesleyan U Spring '18
Adam Munawar Rahman & Bryson Davis
==============================================
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module DRLinear where

import Data.List

data Vec = Vec [Double]

data Mtrx = Mtrx [[Double]]

class Sizable a where
  -- size returns the total number of entries in a matrix or vector
  size :: a -> Int
  -- dim returns the dimensions of a matrix or vector as an ordered pair
  dim :: a -> (Int, Int)

instance Sizable Mtrx where
  dim (Mtrx []) = (0, 0)
  dim (Mtrx m@(a:_)) = (length m, length a)
  size a = let (r, c) = dim a in
             r * c

instance Sizable Vec where
  dim (Vec v) = (length v, 1)
  size (Vec v) = length v

class Multable a b where
  -- performs matrix multiplication using vectors and matrices
  mult :: a -> b -> Maybe Mtrx

instance Show Mtrx where
  show (Mtrx x) = "[\n" ++ (unlines $ map (unwords . map show) x) ++ "]"

instance Show Vec where
  show (Vec x) = "[" ++ unwords (map show x) ++ "]"

-- returns the dot product of two vectors of the same size, "Nothing" otherwise
dot :: Vec -> Vec -> Maybe Double
dot w1@(Vec v1) w2@(Vec v2) =
            if (dim w1) == (dim w2)
            then Just (dot' v1 v2)
            else Nothing

dot' :: [Double] -> [Double] -> Double
dot' [] [] = 0
dot' (v:ve) (w:we) = (v * w) + dot' ve we

instance Multable Vec Vec where
  -- multiplication of a vector by a vector performs dot product multiplication
  -- and returns a singleton matrix, otherwise nothing if the vectors are not the same size
  mult a b = let x = dot a b in
               case x of
                 Just(b) -> Just(Mtrx [[b]])
                 Nothing -> Nothing

-- finds entry (r, c) in the given matrix
index :: Mtrx -> (Int, Int) -> Double
index (Mtrx a) (r, c) = a !! (r - 1) !! (c - 1)

-- checks whether the matrix is the of dimensions (r, c)
check :: Mtrx -> (Int, Int) -> Bool
check (Mtrx []) r = r == (0,0)
check (Mtrx m) (r,c) = (length m == r) && (check' m c)

check' :: [[a]] -> Int -> Bool
check' [] z = z == 0
check' (v:vs) z = (length v == z) && (check' vs z)

-- performs scalar multiplication on a matrix
scMult :: Mtrx -> Double -> Mtrx
scMult (Mtrx m) c = Mtrx (map (\x -> map (\y -> y * c) x) m)

class Addable a where
  -- adds vectors or matrices of the same size, returns "Nothing" otherwise
  add :: a -> a -> Maybe a

addV :: Vec -> Vec -> Maybe Vec
addV (Vec v) (Vec w) = if size (Vec v) == size (Vec w)
                       then Just (Vec (addV' v w))
                       else Nothing

addV' :: [Double] -> [Double] -> [Double]
addV' [] [] = []
addV' (v:vs) (w:ws) = (v + w) : (addV' vs ws)

addM :: Mtrx -> Mtrx -> Maybe Mtrx
addM a@(Mtrx m) b@(Mtrx n) = if dim a == dim b
            then Just (Mtrx (add' m n))
            else Nothing

add' :: [[Double]] -> [[Double]] -> [[Double]]
add' [] [] = []
add' (a:as) (b:bs) = (add'' a b):(add' as bs)

add'' :: [Double] -> [Double] -> [Double]
add'' [] [] = []
add'' (a:as) (b:bs) = (a + b):(add'' as bs)

instance Addable Mtrx where
  add = addM

instance Addable Vec where
  add = addV


-- returns the determinant of a square matrix
-- returns 0 for nonsquare matrices and those larger than 4x4
-- only implemented on matrices of size 1x1 -> 4x4
det :: Mtrx -> Double
det (Mtrx a) =
  let (x,y) = dim (Mtrx a) in
    if (x == y) && size (Mtrx a) <= 16
      then det' a
      else 0.0

det' :: [[Double]] -> Double
det' [[a]] = a
det' [[a, b], [c, d]] = a*d - b*c
det' [[a,b,c], [d,e,f], [g,h,k]] = g*det'([[b,c],[e,f]])
                                 - h*det'([[a,c],[d,f]])
                                 + k*det'([[a,b], [d,e]])
det'[[a,b,c,d],[e,f,g,h],[k,l,m,n],[o,p,q,r]] = a*det'([[f,g,h],[l,m,n],[p,q,r]])
                                              - e*det'([[b,c,d],[l,m,n],[p,q,r]])
                                              + k*det'([[b,c,d],[f,g,h],[p,q,r]])
                                              - o*det'([[b,c,d],[f,g,h],[l,m,n]])

-- returns the column c of 2d array a
getColumn :: [[Double]] -> Int -> [Double]
getColumn a c = map (\x -> x !! (c - 1)) a

-- returns the row r of 2d array a
getRow :: [[Double]] -> Int -> [Double]
getRow a r = a !! (r - 1)

instance Multable Mtrx Mtrx where
  mult a@(Mtrx m) b@(Mtrx n) = let (_, x) = dim a
                                   (y, _) = dim b in
             if x == y
             then Just (Mtrx (mult' m n))
             else Nothing

mult' :: [[Double]] -> [[Double]] -> [[Double]]
mult' a b = [[ sum $ zipWith (*) a' bt | bt <- (transpose b) ]| a' <- a ]

-- when a vector is multiplied by a matrix on the
-- right, it is treated as a column vector
instance Multable Mtrx Vec where
  mult m@(Mtrx a) v@(Vec b) =
    let (x,y) = dim m
        z = size v in
    if y == z
    then Just(Mtrx[map (\x -> x `dot'` b ) a])
    else Nothing

-- when a vector is multiplied by a matrix on the
-- left it is treated as a row vector
instance Multable Vec Mtrx where
  mult v@(Vec b) m@(Mtrx a) =
    let (x,y) = dim m
        z = size v in
    if x == z
    then Just(Mtrx[map (\r -> r `dot'` b) (map (\c -> getColumn a c) [1..y])])
    else Nothing
