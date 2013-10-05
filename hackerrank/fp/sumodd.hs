--Find sum of all odd elements of a list
f = sum . dropAll isEven -- Fill up this function body

dropAll :: (a -> Bool) -> [a] -> [a]
dropAll f x = g f x []
    where   g f [] c = c
            g f (x:xs) c = g f xs (h f x c)
            h f x c = if (f x) then c else x:c

f' = sum . filter (not . isEven) -- Efficient implementation


isEven :: Int -> Bool
isEven n    | (n`mod`2)==1 = False
            | otherwise  = True

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
   inputdata <- getContents
   putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata
