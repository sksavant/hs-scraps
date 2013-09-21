-- 1
-- Last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast(xs)

-- 2
-- Last but one element of a list
myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (x:xs) = myButLast(xs)

-- 3
-- Kth element of a list
elementAt :: [a] -> Int -> a
elementAt x i = x !! (i-1)

-- 4
-- Number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1+myLength(xs)

-- 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse a = myr a []
    where   myr [] a = a
            myr (x:xs) a = myr xs (x:a)

-- 6
-- Check if palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = (a == (myReverse a))

-- 7
-- Flattening a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a  -> [a]
flatten (Elem a) = [a]
flatten (List a) = concat [(flatten x) | x <- a]

-- 8
-- Eliminate conseutive dups of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:(y:xs)) | x==y = compress (x:xs)
                    | otherwise = x:compress(y:xs)
-- Using foldr
compress' x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x


-- 9
-- Consecutive duplicates of lists into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:(y:xs)) | x==y = f x (pack (y:xs))
                | otherwise = [x]:(pack (y:xs))
    where f t (x:xs) = (t:x):xs
-- Using foldr
pack' :: (Eq a) => [a] -> [[a]]
pack' = foldr func []
    where   func x [] = [[x]]
            func x (y:xs) = if x == (head y) then ((x:y):xs) else ([x]:y:xs)

-- 10
-- Run-length encoding on list
encode :: Eq a => [a] -> [(Int,a)]
encode x = [(length a,head a)| a <- (pack x)]


-- 11
-- Modified run-length encoding
data EncType a = Multiple Int a | Single a
    deriving Show
encodeModified ::(Eq a) => [a] -> [EncType a]
encodeModified x = [f a | a <- (pack x)]
    where f a   | (length a)==1 = Single (head a)
                | otherwise = Multiple (length a) (head a)

-- 12
-- Decoding the run-length encoding
decodeModified :: (Eq a) => [EncType a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (f x) ++ decodeModified(xs)
    where   f (Single c) = [c]
            f (Multiple n c) = replicate n c

-- 13
--
