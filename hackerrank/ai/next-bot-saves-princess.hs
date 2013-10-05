module Main where

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
-- Head ends here
nextMove :: Int -> [String] -> String
nextMove n board = moveinpathFromTo (xm,ym) (xp,yp)
    where   (xm,ym) = findM board
            (xp,yp) = findP board

findM :: [String] -> (Int,Int)
findM board = find 'm' board

findP :: [String] -> (Int,Int)
findP board = find 'p' board

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
main = do

    n <- getLine
    xy <- getLine

    let i = read n
    grid <- getList i

    putStrLn.nextMove i $ grid
