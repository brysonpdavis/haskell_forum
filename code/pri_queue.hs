data PriQueue a = Empty | Node (PriQueue a) (Int, a) (PriQueue a)

qInsert :: a -> Int -> PriQueue a -> PriQueue a
qInsert x r Empty = Node (Empty)(r, x)(Empty)
qInsert x r (Node (lt)(i, y)(rt)) =
    case compare r i of
      LT -> Node (qInsert x r lt)(i, y)(rt)
      _ -> Node (lt)(i, y)(qInsert x r rt)

-- returns the lowest ranked/highest priority element

qGet :: PriQueue a -> Maybe a
qGet Empty = Nothing
qGet ( Node (Empty)(_, x)(_) ) = Just(x)
qGet ( Node (lt)(_, _)(_) ) = qGet lt

-- returns a tree with the lowest ranked/highest priority element removed

qCut :: PriQueue a -> PriQueue a
qCut Empty = Empty
qCut (Node(Empty)(_, _)(rt)) = rt
qCut (Node(lt)(i, x)(rt)) = Node(qCut(lt))(i, x)(rt)

qMap :: (a -> b) -> PriQueue a -> PriQueue b
qMap f Empty = Empty
qMap f (Node (lt)(i, x)(rt)) = Node (qMap f lt) (i, (f x)) (qMap f rt)

qShow :: (Show a) => PriQueue a -> String
qShow Empty = ""
qShow (Node(lt)(i, x)(rt)) = qShow(lt) ++ "Rank: " ++ show i ++ "  Item: " ++ show x ++ "\n" ++ qShow(rt)

instance (Show a) => Show (PriQueue a) where
  show p = qShow p

instance Functor PriQueue where
  fmap f p = qMap f p


q :: PriQueue String
q = Empty
q2 = qInsert ("do dishes") 3 q
q3 = qInsert ("do haskell homework") 1 q2
q4 = qInsert ("eat dinner") 2 q3
