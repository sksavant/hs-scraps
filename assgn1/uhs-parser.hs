-- <Not Complete>
-- Author : S Krishna Savant <savant.2020@gmail.com>
-- Parser for u-haskell : Major Assignment 1
import Data.List
import Data.Char

type FName = String
type Var = String

data Program    = Prog [Fundef] Exp deriving Show
data Fundef     = Fun String [String] Exp deriving Show
data Exp        = I Int
                | V Var
                | B Bool
                | Nil
                | FName String
                | App Exp Exp
    deriving Show

-- parse x is the wrapper
-- First break with '\n' into a list of strings with lines
-- On second thought let's think. 
-- We need to return a Prog with a list of Fundefs and a Exp
parse :: String -> Program
parse x = Prog (fst y) (snd y)
    where y =parseList (lines x)

parseList :: [String] -> ([Fundef],Exp)
parseList [] = ([],Nil)
parseList (x:xs)    | isExp(x) = (fst ys,f (just expr x))
                    | isFundef(x) = (y:(fst ys),snd ys)
    where   ys = parseList xs
            y = g (findfundef x)

--Prog (lsfun x) (f (map findexpr (lines x)))

-- isExp
isExp :: String -> Bool
isExp x = True

-- findexpr x will see if there is any expression and returns the main expr
fact :: Parser Char Exp
fact    =  sp integer <@ I
        <|> sp bool <@ B
        <|> (sp identifier)
            <*> (spaceList expr) <@ ffn
        <|> parenthesized expr
        <|> (bracketed (commaList expr)) <@ g
--  where   fcl x,el) = App x (g el)
    where   g [] = Nil
            g (x:xs) = App (App (FName "cons") x) (g xs)
            ffn (x,el) = App (FName x) (g' el )
                where   g' (x:xs) = App x (g' xs)
                        g' [] = Nil

expr :: Parser Char Exp
--expr = sp expr'
expr = chainr fact
        (   symbol '+' <@ f1
        <|> symbol '-' <@ f2
        <|> symbol ':' <@ f3
        )
    where   f c e1 e2 = App (App (FName c) e1) e2
            f1 c e1 e2 = f "+" e1 e2
            f2 c e1 e2 = f "-" e1 e2
            f3 c e1 e2 = f "cons" e1 e2

isFundef :: String -> Bool
isFundef x = False

findfundef :: Parser Char Fundef
findfundef = succeed (Fun "check" [] Nil)

bool :: Parser Char Bool
bool =  token("True") <@ (\x -> True)
        <|> token("False") <@ (\x -> False)

f :: [([Char],Exp)] -> Exp
f p = snd (p!! 0)
--f p | length(p)>0   = snd ( p !! 0)
--    | otherwise     = Nil

g :: [([Char],Fundef)] -> Fundef
g p = snd ( p !! 0)

-- lsfun x will generate the list of all Fundef s in the program
lsfun :: String -> [Fundef]
lsfun x = [Fun "check" [] Nil]

main = do 
--    input <- readFile "pfile"
    let
        input = "3+4+(fib 4)"
        --input = "False+[4,5,6]"
        --input = "fib 5 8"
        --input = "4 + (5 - 6) + (fib 8)"
    let
        program = parse input
        --program = (sp integer) " "
        --program =  (just ( identifier <*> spaceList expr) )input
        --program = bracketed (commaList expr) input
    print program

-- Copied from PFunc

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
--sp  ::  Parser Char a -> Parser Char a
--sp   =  (greedy (satisfy isSpace) *> )

--sp'  =  ( . (dropWhile isSpace))


sptoken  ::  String -> Parser Char String
sptoken   =  sp . token

spsymbol ::  Char -> Parser Char Char
spsymbol  =  sp . symbol

spident  ::  Parser Char String
spident   =  sp identifier

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
data Tree = Nill
          | Bin (Tree, Tree)
    deriving Show
parens :: Parser Char Tree
-- Initial formulation : not compiling
--parens =    ( symbol '('
--            <*> parens
--            <*> symbol ')'
--            <*> parens
--            )   <@ (\(_,(x,(_,y))) -> Bin(x,y))
--            <|> epsilon <@ const Nill

-- alternate formulation
p <* q = p <*> q <@ fst
p *> q = p <*> q <@ snd
open = symbol '('
close = symbol ')'

parens = (open *> parens <* close) <*> parens <@ Bin
        <|> succeed Nill

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
bracketed p = pack (symbol '[') p (symbol ']')
compound p = pack (token "begin") p (token "end")

-- listOf generates parser for list given parser for items and parser for seperator
listOf p s = p <:*> many (s *> p) <|> succeed []

commaList p = listOf p (symbol ',')
spaceList p = listOf p (symbol ' ')

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
            f ([_],n) = -n

integer'    ::  Parser Char Int
integer'     =  (option (symbol '-') <?@ (id,const negate)) <*> natural  <@ ap
    where ap (f,x) = f x

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
