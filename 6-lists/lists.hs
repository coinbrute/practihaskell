isPalindrome :: [Char] -> Bool
isPalindrome word = word == reverse word

respond :: [Char] -> [Char]
respond phrase = if '!' `elem` phrase
                 then "Wow!"
                 else "uh...okay"

takeLast :: Int -> [a] -> [a]
takeLast n aList = reverse (take n (reverse aList))

ones :: Num a => Int -> [a]
ones n = take n (cycle [1])

assignToGroups :: Int -> [a] -> [(Int,a)]
assignToGroups n aList = zip groups aList
 where groups = cycle [1..n]


-- Q6.1
-- Haskell has a function called repeat that takes a value and repeats it infinitely. Using the functions you've learned so far, implement your own version of repeat
rpt :: a -> [a]
rpt value  = cycle [value]

-- Q6.2
-- Write a function subseq that takes three arguments: a start position(inclusive), and end position(exclusive), and a list. The funciton should return the subsequence between the start and end.
-- For Example: 
 -- subseq 2 5 [1,2,3,4,5,6,7,8,9,10]
 -- [3,4,5]
 -- subseq 2 7 "a puppy"
 -- puppy
subseq :: Int -> Int -> [a] -> [a]
subseq start end aList = take (end - start) (drop start aList)

-- Q6.3
-- Write a function inFirstHalf that returns True if an element is in the first half of a list and otherwise returns False.

infirstHalf _  [] = False
inFirstHalf el xs 
                 | el `elem` firstHalf = True
                 | otherwise           = False
 where firstHalf = take ((length xs) `div` 2) xs