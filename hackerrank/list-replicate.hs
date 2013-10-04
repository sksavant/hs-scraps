-- f takes a number an list as input and 
f n x = concat ( g n x) --concat . g

g n = map (\x -> concat ( take n ( repeat [x]))) -- Complete this Function


-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main = do
   n <- readLn :: IO Int
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f n $ map (read :: String -> Int) $ lines inputdata
