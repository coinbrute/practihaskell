import Data.Char

add3ToAll :: Num a => [a] -> [a]
add3ToAll [] = []
add3ToAll (x:xs) = (3+x):add3ToAll xs

mul3ToAll :: Num a => [a] -> [a]
mul3ToAll [] = []
mul3ToAll (x:xs) = (3*x):mul3ToAll xs

doListFunc :: Num a => ([a] -> [a]) -> [a] -> [a]
doListFunc _ [] = []
doListFunc f xs = f xs


-- will be recreating the following examples
{-
 GHCi> map ("a "++) ["train", "plain", "boat"]
 ["a train", "a plane", "a boat"]
 GHCi> map (^2) [1,2,3]
 [1,4,9]
-}
addAnA        :: [[Char]] -> [[Char]]
addAnA []     = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

squareAll        :: [Int] -> [Int]
squareAll []     = []
squareAll (x:xs) = x^2 : squareAll xs

myMap          :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = (f x) : myMap f xs

myFilter             :: (a -> Bool) -> [a] -> [a]
myFilter test []     = []
myFilter test (x:xs) = if test x
                       then x : myFilter test xs
                       else myFilter test xs

myRemove             :: (a -> Bool) -> [a] -> [a]
myRemove test []     = []
myRemove test (x:xs) = if test x
                       then myRemove test xs
                       else x : myRemove test xs

myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

revCons x y = y : x
myReverse xs = foldl revCons [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
 where newInit = f init x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
 where rightResult = myFoldr f init xs

-- Q9.1 
-- Use filter and length to re-create the elem function
myElem _ [] = False
myElem e xs = length (myFilter (== e) xs) > 0

-- Q9.2
-- Your isPalindrome function from lesson 6 doesn't handle sentences with spaces or capitals. Use map and filter to make sure the phrase "A man a plan a canal Panama" is recognized as a palindrome.
isPalindrome [] = False
isPalindrome xs = processedXs == reverse processedXs
 where filtered = filter (/= ' ') xs
       processedXs = map toLower filtered

-- Q9.3
-- In mathematics, the harmonic series is the sum of 1/1 + 1/2 + 1/3 + 1/4 ... Write a function harmonic that takes an argument n and calculates the sum of the series to n. Make sure to use lazy evaluation.
harmonic n = sum (take n values)
 where pairs = zip (cycle [1.0]) [1.0,2.0..]
       values = map (\pair -> (fst pair)/(snd pair)) pairs

