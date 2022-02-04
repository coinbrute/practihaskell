-- Rule 1 Identify end goals

-- Rule 2 Determine what happens when a goal is reached

-- Rule 3 List all alternate possibilities

-- Rule 4 Determine your "Rinse and Repeat"

-- Rule 5 Ensure that each alternative moves you toward the goal

-- Example using Euclids Algorithm 
-- GCD is the greatest common divisor of two numbers
-- the GCD of two numbers is the largest number that evenly divides them both 
-- GCD for 20 and 16 is 4
-- GCD for 10 and 100 is 10
-- Rundown:
 -- start with two numbers a and b
 -- if you divide a by b and the remainder is 0, clearly b is the GCD
 -- otherwise, you change the value of a by assigning it the value of b (b becomes the new a). You also change the value of b to be the remainder you obtained in step 2 (the new b is the remainder of the original a divided by the original b).
 -- then repeat until a/b has no remainder.
-- Breakdown:
 -- a = 20, b = 16
 -- a/b = 20/16 = 1 remainder 4
 -- a = 16, b = 4
 -- a/b = 16/4 = 4 remainder 0
 -- GCD = b = 4

-- Rule 1 for GCD 
 -- no remainder for a/b
 -- a `mod` b == 0

-- Rule 2 for GCD
 -- if a `mod` b == 0
 -- then b ...

-- Rule 3 for GCD
 -- else gcd b (a `mod` b)

-- put it all together 

gcd' :: Int -> Int -> Int
gcd' a b = if a `mod` b == 0
          then b
          else gcd' b (a `mod` b)


myTail :: [a] -> [a]
myTail []     = []
myTail (_:xs) = xs

-- Q7.2 Rewrite myGCD by using pattern matching 
myGCD :: Int -> Int -> Int
myGCD a 0 = a -- if b is 0 return a 
myGCD a b = myGCD b (a `mod` b) -- otherwise flip b and a and call myGCD on b with the mod of a and b