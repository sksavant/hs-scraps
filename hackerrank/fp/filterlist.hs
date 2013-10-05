-- Filter the list to leave only numbers more than n
f n = filter (\x -> (x<n)) --Fill up this function

-- The Input/Output section. You do not need to change or modify this part
main = do
   n <- readLn :: IO Int
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f n $ map (read :: String -> Int) $ lines inputdata
