module PFunc where
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
digit :: Parser Char Int
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
    where f (x,y) = (1+x) `max` y

-- foldparens is generalized version
foldparens :: ((a,a)->a) -> a -> Parser Char a
foldparens f e = p
    where p = (open *> p <* close) <*> p <@ f
            <|> succeed e
-- parens = foldparens Bin Nil
-- nesting = foldparens f 0

p1 <:*> p2 = p1 <*> p2 <@ (\(x,xs)->x:xs)
            <|> succeed []

-- many p => 0 or more occurences of that construction
many p = p <:*> many p

-- natural will parse the natural number
natural :: Parser Char Int
natural = many digit <@ foldl f 0
    where f a b = 10*a+b

many1 p = p <*> many p <@ list
    where list (x,xs) = x:xs

-- option list with 0 or 1 element
option p = p <@ (\x->[x])
        <|> succeed []

-- pack : Generalized parser for opening token, body, closing token
pack s1 p s2 = s1 *> p <* s2

parenthesized p = pack (symbol '(') p (symbol ')')
compound p = pack (token "begin") p (token "end")

-- listOf generates parser for list given parser for items and parser for seperator
listOf p s = p <:*> many (s *> p) <|> succeed []

commaList p = listOf p (symbol ',')

-- Saving the seperator as well
chainl p s = p <*> many (s <*> p) <@ f
    where f = uncurry (foldl (flip ap2))

chainr p s = many (p <*> s) <*> p
            <@ uncurry (flip (foldr ap1))

ap2 (op,y) = (`op` y)
ap1 (x,op) = (x `op`)

-- Using the <?@ operator
p <?@ (no,yes) = p <@ f
    where   f [] = no
            f [x] = yes x

-- fract : fractional number
fract :: Parser Char Float
fract = many digit <@ foldr f 0.0
    where f d x = (x + fromIntegral d)/10.0

--optional fractional part
fixed = (integer <@ fromIntegral)
        <*>
        (option (symbol '.' *> fract) <?@ (0.0, id))
        <@ uncurry (+)

integer :: Parser Char Int
integer = option (symbol '-') <*> natural <@ f
    where   f ([],n) = n
            f (_,n) = -n

-- identifier 
identifier = many1 (satisfy isAlpha)

-- first yields only first possibility
first p xs  | null r = []
            | otherwise = [head r]
                where   r = p xs

-- greedy : take all or none from manyr
greedy = first.many
greedy1 = first.many1

-- accepts the construction if present but does not fail if not present
compulsion = first.option

main1 = do
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
        x6 = parenthesized (many symbola) "(aaa)vfbgddd"
    let
        asserted = ((x==x0) && (x==z) && (x==y) && (x==[("sfavb",'a')]))
    print x6
