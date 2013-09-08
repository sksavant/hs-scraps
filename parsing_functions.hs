type Parser a  b = [a] -> [([a],b)]

-- symbola parses the string for 'a'
symbola :: Parser Char Char
symbola []                      = []
symbola (x:xs)  | x=='a'        = [(xs,'a')]
                | otherwise     = []

-- Generalizing
symbol a []             = []
symbol a (x:xs) | a==x  = [(xs,x)]
                | otherwise = []

-- token takes a fixed string of characters
token k cs  | k== take n xs = [(drop n xs, k)]
            | otherwise     = []
                where n = lenght k

main = do
    let
        x = symbola "asfavb"
    let
        y = symbol 'a' "asfavb"
    let

        asserted = ((x==y) && (x==[("sfavb",'a')]))
    print asserted







o
