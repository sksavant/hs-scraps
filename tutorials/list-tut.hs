-- Problem set on Lists
-- 
-- prob1 =
    --a1 = ([[]] ++ xs == xs)} -- False
    --a2 = ([[]] ++ xs == [xs]) -- False
    --a3 = [[]] ++ xs == [[],xs] -- False
    --a4 = [[]] ++ [xs] == [[],xs] -- True
    --a5 = [xs] ++ [] == [xs] -- True
    --a6 = [xs] ++ [xs] == [xs,xs] -- True

-- prob2 @TODO
errno :: String -> Int
errno (x:xs) = 0

-- prob 3
powers :: Int -> [Int]
powers x = 1:(f x (powers x))
    where f x (lx:ls) = x*lx:(f x ls)

powers' :: Int -> [Int]
powers' x = 1:(f x)
    where f x = [x*cx| cx <- powers' x]

-- prob 4
-- (a)
( ++- ) :: [a] -> [a] -> [a]
[] ++- l = l
(x:xs) ++- l = x : (xs ++- l)
-- In l1 ++ l2 ,  Work done is equal to length(l1)
--
-- (b)
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-- In reverse l, work done is \sum k k=1 to n-1
-- (c)
reverseff l = rev l []
    where   rev [] a = a
            rev (x:xs) a = rev xs (x:a)

-- prob 5 @TODO
hof a b c [] = b
hof a b c (x:xs) = c ( a x ) (hof a b c xs)

-- prob 6
-- Longest common subsequence
lcs :: (Eq a) => [a] -> [a] -> [a]
ans1 = lcs ['A','B','C','B','D','A'] ['B','D','C','A','B','A']
lcs [] b = []
lcs a b = longestlist (commonseq a b)
commonseq a b = [xs | xs <- (subseqs a), ys<- (subseqs b), xs==ys]

longestlist [[]] = []
longestlist a = f a 0 []
    where   f [] n a = a
            f (x:xs) n a    | (length x) > n = f xs (length x) x 
                            | otherwise = f xs n a

subseqs [] = [[]]
subseqs (x:xs) = [x:e | e <- (firstseq xs)] ++ subseqs(xs)

firstseq :: [a] -> [[a]]
firstseq [] = [[]]
firstseq (x:xs) = []:[x:e | e <- (firstseq xs)]

-- prob 7 @TODO
type Graph = [(Int,Int)]
type Node = Int
type Path = [Int]

-- makepath returns all simple paths from a node in graph
-- makepath :: Node -> Graph -> [Path]
-- makepath n g = concat [nocyclepath n x | x <- [(makepath nn g) | nn <- (neighbors n)]]

-- prob 8
-- cprod : cartesian product of list of lists
cprod :: [[a]] -> [[a]]
cprod (x:[]) = [[xn] | xn <- x]
cprod (x:xs) = [xn:yn | xn <- x, yn <- (cprod xs)]

-- prob 9
--
