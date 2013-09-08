-- Parse x is the wrapper around parse which returns the parsed function
parse x = x

main = do 
    input <- readFile "pfile"
    let
        program = parse input
    print program
