f lst = map (snd) ( filter (\(x,y) -> (isOdd x)) (zip [0..] lst) )-- Fill up this Function

isOdd :: Int -> Bool
isOdd n    | (n`mod`2)==0 = False
           | otherwise  = True

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
