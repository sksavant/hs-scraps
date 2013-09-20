-- Problem set on trees
-- prob 1
data Tree a = Node (Tree a) a (Tree a) | Null
    deriving Show
dia :: Tree a -> Int
dia t  = 0
