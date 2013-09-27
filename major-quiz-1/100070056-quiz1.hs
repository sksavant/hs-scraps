import Data.Ratio
-- Author : S Krishna Savant 
-- Roll no :100070056
-- CS 613 Major Quiz 1 09/21
-- Q 2
avg l = (sum l) / ((length l)%1)
-- Using the Ratio data Type we convert Int of length l to a Ratio Integral

-- Q 3 @TODO
--
foldn f n c id	| n==c = id
				| otherwise = f n c (foldn f (n+1) c id) 

mytan x k = foldn f 0 k 0
	where f n c k = x/(((2*n+1)) - k)

-- Q 4 
-- Sequence of frations can be used as approximations for real number
x = map a [0,1..] 
sqrtM :: Rational -> [Rational]
sqrtM p = [a n p |n <- [0,1..]]
a :: Int -> Rational -> Rational
a n p 	| n==0 = (anot p)
		| otherwise = (1%2) * ((a (n-1) p) + ((p)/(a (n-1) p)))

anot :: Rational -> Rational
anot p = head (dropWhile (\x -> (x*x)<=p) [1..]) - 1

--(//) :: Rational -> Rational -> Rational
--x // y = (numerator x * denominator y)%(numerator y * denominator x)

--(**) :: Rational -> Rational -> Rational
--x ** y = (numerator x * numerator y)%(denominator x * denominator y)

--(++) :: Rational -> Rational -> Rational
--x ++ y = (numerator x * denominator y + numerator y * denominator x)%(denominator x * denominator y)

-- Q 5
-- 
max_scount :: (Ord a) => [a] -> [Int]
max_scount [] = []
max_scount (x:xs) = (findcount x xs):max_scount(xs)
	where findcount x xs = sum [if (c > x) then 1 else 0| c <- xs]

-- Q 6
-- 
data Gtree = Gnode Int [Gtree]
    deriving Show

sumtree :: Gtree -> Int
sumtree t = sum [(toDigit x)|x <- (traverse t)]
    
toDigit :: [Int]->Int
toDigit na = h na (length na)
    where h x n = sum [((x !! i)*(pow 10 (n-i-1)))| i <- [0..(n-1)]] 

pow m n = h m n 1
    where   h m 0 k = k
            h m n k = h m (n-1) (k*m)

traverse :: Gtree -> [[Int]]
traverse (Gnode x []) = [[x]]
traverse (Gnode x tree) = [concat [x:i1 |i1 <-(traverse t)] )| t <- tree]



