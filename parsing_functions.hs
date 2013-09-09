import Data.Char -- for isDigit
--import Data.Tree

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
token k xs  | k== take n xs = [(drop n xs, k)]
            | otherwise     = []
                where n = length k

-- satisfy is generalization of symbol
satisfy p []            = []
satisfy p (x:xs)        = [(xs,x)| p x]

-- epsilon, the empty parse
epsilon xs = [(xs,())]

-- succeed, gives a fixed value for whatever input
succeed v xs = [(xs,v)]

-- fail always returns a empty list of successes
fail xs = []

-- Parser combinators
-- <*> sequential composition of parsers
(p1 <*> p2) xs = [ (xs2, (v1,v2))
                 | (xs1,v1) <- p1 xs
                 , (xs2,v2) <- p2 xs1]

-- <|> alternative composition
(p1 <|> p2) xs = p1 xs ++ p2 xs

-- Parser transformers
-- sp will drop spaces from the input and then applies given parser
sp p = p . dropWhile(==' ')

-- just will do same as p but ensures that the rest of string is null
just p = filter (null.fst) . p

-- <@ will apply a function on the parse tree  
(p <@ f) xs = [ (ys, f v)
                | (ys,v) <- p xs]

-- digit transform output to integer
digit = satisfy isDigit <@ f
    where f c = ord c - ord '0' -- ord c gives the ascii value of character c

-- some "deterministic parser" : parses text, guarantees empty rest string, picks 
-- first solution and delivers parse tree only
type DetPars symbol result = [symbol] -> result
some :: Parser s a -> DetPars s a 
some p = snd . head . just p

-- Matching parentheses
-- Let's define Tree structure
data Tree = Nil
          | Bin (Tree, Tree)
    deriving Show
parens :: Parser Char Tree
-- Initial formulation : not compiling
--parens =    ( symbol '('
--            <*> parens
--            <*> symbol ')'
--            <*> parens
--            )   <@ (\(_,(x,(_,y))) -> Bin(x,y))
--            <|> epsilon <@ const Nil

-- alternate formulation
p <* q = p <*> q <@ fst
p *> q = p <*> q <@ snd
open = symbol '('
close = symbol ')'

parens = (open *> parens <* close) <*> parens <@ Bin
        <|> succeed Nil

-- Set priorities of operators
infixr 6 <*>, <*, *>
infixl 5 <@
infixr 4 <|>

-- nesting : calculates the nesting depth of nested parentheses
nesting :: Parser Char Int
nesting = (open *> nesting <* close) <*> nesting <@ f
          <|> succeed 0
    where f (x,y) = (1+x) 
    ''a`

main = do
    let
        x = symbola "asfavb"
    let
        y = symbol 'a' "asfavb"
    let
        z = satisfy f "asfavb"
            where f x = (x=='a')
    let
        x0 = sp symbola "  asfavb"
    let
        x1 = parens "s(abc)"
    let
        asserted = ((x==x0) && (x==z) && (x==y) && (x==[("sfavb",'a')]))
    print x1








