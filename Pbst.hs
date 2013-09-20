
-- Binary Search tree
data Stree a = Null | Fork a (Stree a) (Stree a)
    deriving Show
flatten' :: Stree a -> [a]
flatten' Null = []
flatten' (Fork x xt yt) =  flatten' xt ++ [x] ++ flatten' yt

insert x Null = Fork x Null Null
insert x t@(Fork y xt yt)   | x<y = Fork y (insert x xt) yt
                            | x==y = t
                            | x>y = Fork y xt (insert x yt)
