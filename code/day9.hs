data Void
data Not a = MkNot(a -> Void)

test :: (Either (a, b) (a, c)) -> (a, Either b c)
test (Left (a,b)) = (a, Left b)
test (Right (a,c)) = (a, Right c)

test2 :: (a, Either b c) -> Either (a,b) (a,c)
test2 (a, Left b) = Left (a, b)
test2 (a, Right c) = Right (a, c)

dni :: a -> Not (Not a)
dni x = MkNot $ (MkNot na) -> na x

dne :: Not (Not a) -> a
dne f = undefined

cps :: (a -> b) -> (Not b -> Not a)
cps f = \(MkNot nb) -> MkNot (\a -> nb (f a))

contra2 :: (Not b -> Not a) -> (a -> b)
contra2 f = \a -> dne $ contra f $ dni a


impl :: (a->b, b->c) -> (a->c)
impl (ab, bc) = bc . ab
