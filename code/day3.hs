map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

data Tree a = Empty | Node (Tree a) a (Tree a)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Node l x r) = Node (treeMap f l) (f x) (treeMap f r)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)

{-
 -Kinds-

Int :: *
Vec3 :: *
Tree :: * -> *

fmap id x = x
map id xs = xs
fmap (g . f) x = fmap g (fmap f x)
-}

instance Functor Tree where
    fmap f x = treeMap f x
