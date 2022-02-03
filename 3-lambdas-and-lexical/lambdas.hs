
lambda :: Integer -> Integer
lambda = (\x -> x*2) -- yes I know I assigned a lambda to a definition. just so I can have it saved and still use it in GHCi

sumSquareOrSquareSumL :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSumL x y = (\sumSquare squareSum ->
                             if sumSquare > squareSum 
                             then sumSquare 
                             else squareSum) (x^2 + y^2) ((x+y)^2)

sumSquareOrSquareSumW :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSumW x y = if sumSquare > squareSum 
                            then sumSquare 
                            else squareSum
 where sumSquare = x^2 + y^2
       squareSum = (x+y)^2

sumSquareOrSquareSumLI :: (Ord p, Num p) => p -> p -> p
sumSquareOrSquareSumLI x y = let sumSquare = x^2 + y^2
                                 squareSum = (x+y)^2
                            in
                            if sumSquare > squareSum 
                            then sumSquare 
                            else squareSum

doubleDouble :: Num p => p -> p
doubleDouble x = (\dubs -> dubs*2) (x*2)

overwrite :: Num p1 => p2 -> p1
overwrite x = (\x -> 
               (\x ->
                (\x -> x) 4
               ) 3
              ) 2

overwriteLI :: Num p1 => p2 -> p1
overwriteLI x = let x = 2
                 in 
                  let x = 3
                   in 
                    let x = 4
                     in 
                      x

-- Chapter Knowledge Check
-- Q3.1 Practice writing lambda functions by rewriting each function in lesson 3 as a lamba expression.
 -- simple and calcChange
simple :: p -> p
simple = (\y -> y)

calcChange :: (Ord p, Num p) => p -> p -> p
calcChange owed given = (\change ->
                        if change > 0
                        then change
                        else 0) (given - owed)

inc :: Num a => a -> a
inc = (\n -> n+1)

double :: Num a => a -> a
double = (\n -> n*2)

square :: Num a => a -> a
square = (\n -> n^2)

-- Q3.2 Using a alet expression and a lambda funciton aren't exactly the same thing under the hood. For example, the following code will cause an error if you try to run it: 

counter' x = let x = x + 1
             in
              let x = x + 1
              in 
               x

counter :: Num a => a -> a
counter x = (\ x -> x + 1) 
            ((\x -> x + 1)
             ((\x -> x) x))