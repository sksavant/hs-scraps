import Data.Ratio

str :: Int -> String
str n = shows n []

nSol :: Int -> Int
nSol n = length (sols (factorial n))

sols :: Int -> [(Int,Int)]
sols n = [(x,y) | x<-[1..n],y<-[1..n], ((1`div`x) + (1`div`y))==(1`div`n)]

factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

main = do
    n <- readLn :: IO Int
    putStrLn (str ((nSol n) `mod` 1000007) )
