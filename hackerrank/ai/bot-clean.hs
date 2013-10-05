module Main where

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

-- Head ends here
next_move :: String -> [String] -> String
next_move p board = findNextMove (xb,yb) board
    where (xb,yb) = parsePos p

parsePos :: String -> (Int,Int)
parsePos s = tuplify (map read $ words s ::  [Int])

tuplify :: [Int] -> (Int,Int)
tuplify [x,y] = (x,y)
tuplify x = (x!!0,x!!1)

findNextMove :: (Int,Int) -> [String] -> String
findNextMove b board    | (((board !! fst b) !! snd b)=='d') = "CLEAN\n"
                        | otherwise = moveinpathFromTo b d
    where d = find 'd' board

find :: Char -> [String] -> (Int,Int)
find c x = findHelper c x (0,0)
    where   findHelper c (x:xs) (a,b) | c `elem` x = (a,(findHelperInner c x 0))
                                      | otherwise = findHelper c xs ((a+1),b)
            findHelperInner c (x:xs) a  | c==x = a
                                        | otherwise = findHelperInner c xs (a+1)

moveinpathFromTo :: (Int,Int) -> (Int,Int) -> String
moveinpathFromTo (xm,ym) (xp,yp)  | xm>xp && ym>yp = (printCorrect (xm-xp) "UP\n") -- ++ (printCorrect (ym-yp) "LEFT\n")
                            | xm>xp && ym<yp = (printCorrect (xm-xp) "UP\n") -- ++ (printCorrect (yp-ym) "RIGHT\n")
                            | xm<xp && ym<yp = (printCorrect (xp-xm) "DOWN\n") -- ++ (printCorrect (yp-ym) "RIGHT\n")
                            | xm<xp && ym>yp = (printCorrect (xp-xm) "DOWN\n") -- ++ (printCorrect (ym-yp) "LEFT\n")
                            | xm==xp && ym>yp = (printCorrect (ym-yp) "LEFT\n")
                            | xm==xp && ym<yp = (printCorrect (yp-ym) "RIGHT\n")
                            | xm<xp && ym==yp = (printCorrect (xp-xm) "DOWN\n")
                            | xm>xp && ym==yp = (printCorrect (xm-xp) "UP\n")

printCorrect n s = concat (take 1 (repeat s))
-- Tail starts here
main :: IO()
main = do
    -- Take input
    pos <- getLine
    board <- getList 5
    -- Do I/O stuff here.
    putStrLn $ next_move pos board
