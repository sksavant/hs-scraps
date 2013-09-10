-- Author : S Krishna Savant <savant.2020@gmail.com>
-- Parser for u-haskell : Major Assignment 1
import Data.List

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
parse x = lines x

main = do 
    input <- readFile "pfile"
    let
        program = parse input
    print program
