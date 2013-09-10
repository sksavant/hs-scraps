import PFunc
data Expr   = Con Int
            | Var String
            | Fun String [Expr]
            | Expr :+: Expr
            | Expr :-: Expr
            | Expr :*: Expr
            | Expr :/: Expr
    deriving Show

fact :: Parser Char Expr
fact    =   integer <@ Con
        <|> identifier
            <*> (option (parenthesized (commaList expr))
                <?@ (Var, flip Fun))
            <@ ap'
        <|> parenthesized expr
    where ap' (x,f) = f x

term :: Parser Char Expr
term = chainr fact
            (   symbol '*' <@ const (:*:)
            <|> symbol '/' <@ const (:/:)
            )

expr :: Parser Char Expr
expr = chainr term
            (   symbol '+' <@ const (:+:)
            <|> symbol '-' <@ const (:-:)
            )

main = do
    --input <- readFile "afile"
    let
        input = "2+(3-4)/4"
    let
        program = just expr input
    print program
