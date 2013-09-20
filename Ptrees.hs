-- Programs from lecture on BTree
data Btree a = Leaf a | Fork (Btree a ) (Btree a)
    deriving Show
-- Eg:
bt1 :: Btree Int
bt1 = Fork (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) (Fork (Leaf 4) (Leaf 5))

size :: Btree a -> Int
size (Leaf a) = 1
size (Fork lt rt) = 1 +  (size lt) + (size rt)

mirror :: Btree a -> Btree a
mirror (Leaf x) = Leaf x
mirror (Fork lt rt) = Fork (mirror rt) (mirror lt)

flatten :: Btree a -> [a]
flatten (Leaf x) = [x]
flatten (Fork lt rt) = flatten lt ++ flatten rt

