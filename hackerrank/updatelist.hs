-- Enter your code here. Read input from STDIN. Print output to STDOUT
-- Print absolute value of list
--
f :: [Int] -> [Int]
f = map (\x -> if (x<0) then (-1*x) else x) -- Complete this function here
--
-- -- This section handles the Input/Output and can be used as it is. Do not modify it.
main = do
    inputdata <- getContents
    mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
