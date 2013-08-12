r = 5.0 -- radius , a comment
areac {- of circle-} = pi * r ^ 2 -- the formula
--variables donot vary.. WHAT?
--r = 2.0, doesnot work
-- r = r+1 -- this doesnot increment r by 1. Shit!
-- It is a recursive definition of r in itself
-- One good thing:
-- No need to follow order
-- y = x *3 and then x= 3 would work as well as the reverse order
-- Functions 

area r = pi * r^2 -- this is a function named area which takes
-- r as an argument and outputs the area as calculated.
-- Some other functions
double x = 2*x
quadruple x = double (double x) -- call a function in the definition of another
square x = x*x
half x = x/2
