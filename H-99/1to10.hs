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
compress :: [a] -> [a]
compress [] = []
compress [x] = [x]

