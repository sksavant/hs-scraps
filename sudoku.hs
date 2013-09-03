
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
--this acts on a row only
pruneList l = map f l
    where l1 = fixed l
    f l2 = filter (\x -> not(x'elem' l1) l2

--prune takes a matrix of choices and prunes all
-- elem works only if Equality is defined. so not for any arbit type a
prune :: (Eq a) => Matrix [a] -> Matrix [a]

-- Convert to rows, map pruneList to each elem of rows and then convert back to original notation 
pruneBy f = f . (map pruneList) . f

prune = pruneBy boxes . pruneBy cols . pruneBy rows
solver2 = filter (correct) . take 50000 mcp . prune. intialChoices --takes only first 50000

-- Mimimum element which is not singleton
minChoice = minimum . (filter (\= 1)) . concat (map (map length))

expand :: Matrix Choices -> [Matrix Choices]

data Btree a = Leaf a | Fork (Btree a) (Btree a)
--two typess ss   


