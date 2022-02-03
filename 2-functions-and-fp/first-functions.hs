main :: IO ()
main = do
 print $ change $ square $ double $ inc $ doublePlusTwo $ calcChange 1 2


calcChange :: (Ord p, Num p) => p -> p -> p
calcChange owed given = if change > 0
                        then change
                        else 0
 where change = given - owed


doublePlusTwo :: Num a => a -> a
doublePlusTwo x = doubleX + 2
 where doubleX = x * 2

-- Chapter knowledge check
-- Q2.1 You used Haskell's if/then/else expresion to write calcChange. In Haskell, all if statements must include an else component. Given our three rules for functions, why can't you have an if statement all by itself?
{-
 A2.1 -
     Without an else statement there would be no return from the function on certain conditions
-}

-- Q2.2 Write functions named inc, double, squre that increment, double, and square an argument n, respectively
inc :: Num a => a -> a
inc n = n+1

double :: Num a => a -> a
double n = n*2

square :: Num a => a -> a
square n = n^2

-- Q2.3 Write a function that takes a value n. If n is even, the function returns n-2, and if the number is odd, the function returns 3*n+1. To check whether the number is even you can use either Haskell's even function or mod function
change :: Integral a => a -> a
change n = if even n
             then n - 2
             else 3 * n - 1