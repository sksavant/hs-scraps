-- Enter your code here. Read input from STDIN. Print output to STDOUT
string_pascal :: Int -> String
string_pascal n = toStr (pascal n) []

pascal :: Int -> [[Int]]
pascal n = [pascalrow i| i<-[1..n]]

pascalrow :: Int -> [Int]
pascalrow 1 = [1]
pascalrow n = 1:[pascalrow (n-1) !! i + pascalrow (n-1) !! (i+1) | i<-[0..n-3]]++[1]

toStr :: [[Int]] -> String -> String
toStr [] c = c
toStr (x:xs) c = toStr xs (toS1 x c)
    where   toS1 [] c = c++['\n']
            toS1 (x:xs) c = toS1 xs (c++(shows x [])++[' '])

main = do
    n <- readLn :: IO Int
    putStrLn (string_pascal n)
