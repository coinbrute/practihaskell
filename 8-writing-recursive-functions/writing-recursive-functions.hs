myLength    :: [a] -> Int
myLength [] = 0 -- return 0 on empty lists...duh
myLength xs = 1 + myLength (tail xs) -- just add 1 to the length of the tail of xs

myLength2        :: [a] -> Int
myLength2 []     = 0
myLength2 (x:xs) = 1 + myLength2 xs -- using pattern matching to avoid need tail call

myTake _ []     = []
myTake 0 _      = []
myTake n (x:xs) = x : myTake (n-1) xs

myCycle (x:xs) = x :myCycle (xs ++ [x]) -- we need to take the first value then call the function on the tail appended to the first value

-- Ackermann Function 
-- A(m,n)
-- if m = 0 return n+1
-- if n = 0 then A(m-1, 1)
-- if both m != 0 and n != 0 then A(m-1, A(m,n-1))

ackFunc 0 n = n + 1
ackFunc m 0 = ackFunc (m - 1) 1
ackFunc m n = ackFunc (m - 1) (ackFunc m (n-1))

-- Collatz conjecture
-- if n = 1 you are done
-- if n is even repeat with n/2
-- if n is odd repeat with n*3+1

collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)

-- Q8.1
--Implement your own version of reverse, which reverses a list
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Q8.2
-- Calculating fibonacci numbers is perhaps the single most common example of a recursive function. The most straightforward definition is as follows:
{-
 fib 0 = 0
 fib 1 = 1
 fib n = fib (n-1) + fib (n-2)
-}
-- Like the Ackermann function, this implementation quikcly explodes due to the mutually recursive calls. But unlike the Ackermann funciton, there's a much more efficient way to compute the nth fibonacci number. Write a function, fastFib, that can compute the 1000th fibonacci number nearly instantly.
-- HINT: fastFib takes three arguments n1,n2,counter. To calculate the 1000th fibonacci number, you call fastFib 1 1 1000 and for the 5th you'd call fastFib 1 1 5 
-- n1 and n2 are starting values
fastFib _  _  0   = 0
fastFib _  _  1   = 1
fastFib _  _  2   = 2
fastFib n1 n2 3   = n1 + n2
fastFib n1 n2 ctr = fastFib (n1 + n2) n1 (ctr - 1)

{-
fastFib 1 1 5 = fastFib (1+1) 1 (5-1)
so...
fastFib 1 1 5 = fastFib 2 1 4
so...
fastFib 2 1 4 = fastFib (2+1) 2 (4-1)
so...
fastFib 2 1 4 = fastFib 3 2 3
so...
fastFib 3 2 3 = (3+2)
so...
fastFib 3 2 3 = 5
-}


