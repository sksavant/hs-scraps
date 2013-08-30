
type Matrix a = [[a]]
type Board = Matrix Char
-- Missed some stuff
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs
rows = id
cols[] = replicate 9 []
cols (x:xs) = zipWith (:) x (cols xs)
boxes = map unchop . unchop --contd
--
type Choices = [Char]
intialChoices :: Board -> Matrix Choices
intialChoices = map(map fillin)

fillin c    | blank c = allvals 
            | otherwise = [c]

--Cartesian product
--
cp [] = [[]]
cp (x:xs) = [h:t|h<-x, t<-cp 

-- Matrix's cartesian product
mcp = cp . (map cp)
solver1 = filter (correct) . mcp . inti alChoices
solver1 = filter (correct) . take 50000 mcp . intialChoices --takes only first 50000

--fixed takes a list if lists and gives a list of singleton lists' elements in it
fixed :: [[a] -> [a]]
fixed [] = []
fixed ([x]:xs) = x:fixed xs
fixed (_:xs) = fixed xs

-- To do pruning to reduce the time complexity
-- Pruning based on the initial choice
-- Power of lazyness??
--prunelist :: 
--prunelist (x:xs) c = |xx  |xx<-x, cx<-c
pruneList l = map f l































