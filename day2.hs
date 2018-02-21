-- creating datatypes
data Vec3 = MkVec3 {getX :: Float, getY :: Float, getZ :: Float} 

-- defining function on a datatype
addVec3 :: Vec3 -> Vec3 -> Vec3
-- addVec3 u v = MkVec3 ((getX u) + (getX v)) ((getY u) + (getY v)) ((getZ u) + (getZ v))
-- OR
addVec3 (MkVec3 x y z) (MkVec3 x' y' z') = MkVec3 (x + x') (y + y') (z + z')

-- assigning a symbol for function
( <+> ) = addVec3

-- creating a way to display Vec3 in terminal
showVec3 :: Vec3 -> String
showVec3 (MkVec3 x y z) = "<" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ ">"

-- creating type classes
instance Show Vec3 where
  show v = showVec3 v


-- anonymous functions
-- :t \x -> x -- identity
-- :t \(x,y) -> x -- projection


find :: (Eq a) => a -> [a] -> Maybe a
find x [] = Nothing
find x (y:ys) = if x == y then (Just x) else find x ys
