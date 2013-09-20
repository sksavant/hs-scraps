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
-- given 2 arrays of integers, find a subset of maximum elements which have the same relative order in both
connect :: [Int] -> [Int] -> [Int]
connect a b = longestlist (sameordersubsets a b)
sameordersubsets a b = [ax | ax <- (subsets a), bx<- (subsets b), ax==bx]

subsets :: [Int] -> [[Int]]
subsets (x:[]) = [[],[x]]
subsets (x:xs) = f x (subsets xs)
    where   f x (y:ys) = (x:y):y:(f x ys)
            f x [] = []

-- prob 10 @TODO
-- Pascal's triangle is a list of lists


-- prob 11
-- Puzzle : 2 numbers a and b from [2,99] are picked. S knows sum, P knows product
good_nums = [2..99]::[Int]
good_factors p = [(a,b)| a<-good_nums, b<-good_nums, a>=b, a*b==p]
good_summands s = [(a,b)| a<-good_nums, b<-good_nums, a>=b, a+b==s]

singleton :: [a] -> Bool
singleton [] = False
singleton (x:[]) = True
singleton (x:xs) = False

fact1 (a,b) = not (singleton (good_factors (a*b)))
fact2 (a,b) = not (singleton (good_summands (a+b)))
fact3 (a,b) = all fact1 [x|x <- (good_summands (a+b))]
fact4 (a,b) = singleton (filter (==True) [fact3 x|x <- (good_factors (a*b))])
fact5 (a,b) = singleton (filter (==True) [fact4 x|x <- (good_summands (a+b))])
result = [(a,b) | a<-good_nums, b<-good_nums, (fact1 (a,b)) == True, (fact2 (a,b)) == True, (fact3 (a,b)) == True, (fact4 (a,b)) == True, (fact5 (a,b)) == True]
-- Not Working. WHY? @DO fact3 was wrong before. Done

-- prob 12 @TODO
--

-- prob 13 
fewestmoves :: [Int] -> Int
fewestmoves x = fm 0 x

-- fm is the fewest moves from sth square in the grid [Int]
fm :: Int -> [Int] -> Int
fm s x  | s<(length x) = if (x!!s)==0 then 1+ (min (fm (s+1) x) (fm (s+4) x)) --Black
                        else 1+(min (fm (s+1) x) (fm (s+2) x)) --White
        | otherwise = 0

-- prob 14 
-- summands which will take an integer n and produces list containing all ways of writing n as sum of positive integers
summands :: Int -> [[Int]]
summands 1 = [[1]]
--summands n = (tail ( [[1]++x | x <- (summands (n-1))] ++ [x++[1] | x<- (summands (n-1))] ) )++[[n]] -- Incorrect!

-- prob 15
dig = [1..9]
--out = head [[('s',s),('e',e),('n',n),('d',d),('m',m),('o',o),('r',r),('y',y)] |  s <- dig, e <- dig, n <- dig, d <- dig, m <- dig, o <- dig, r <- dig, y <- dig, (d+e) `mod` 10 == e ,(n+r+((d+e)`div`10))`mod`10==e, (e+o+((n+r)`div`10))`mod`10==n, (s+m+((e+o)`div`10))`mod`10==o, (s+m)`div`10==m] -- Pretty basic buggy code

