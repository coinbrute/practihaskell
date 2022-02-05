x :: Int
x = 2
-- x^2000 = 0


y :: Integer
y = 2
-- y^2000 = a huge number

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

-- a list of Char is the same as a String
letters :: [Char]
letters = ['a','b','c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34,74)

firstLastMiddle :: (String,String,Char)
firstLastMiddle = ("Oscar","Grouch",'D')

streetAddress :: (Int, String)
streetAddress = (123, "Main St.")

-- type conversions
half :: Int -> Double
half n = (fromIntegral) n / 2

halve :: Integer -> Integer
halve n = n `div` 2

-- converting to and from strings
-- show converts to a string
printDouble :: Int -> String
printDouble n = show (n*2)
-- read takes a string and converts it to another type 
anotherNumber :: Int
anotherNumber = read "6" 

anotherNumberDbl :: Double
anotherNumberDbl = read "6"


-- type signatures for multiple arg functions
-- funcName  1stArg   2ndArg    3rdArg        return value
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

makeAddressNestedLambda :: Int -> String -> String -> (Int, String, String)
makeAddressNestedLambda = (\number ->
                           (\street ->
                            (\town -> (number,street,town) )))

-- type signatures for first class functions i.e. functions that take and/or return functions 
ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n 
             then f n
             else n

-- type variables for any type to be passed in
simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)

-- Q11.1 
-- what is the type signature for filter? How is it different from map
-- filter :: (a -> Bool) -> [a] -> [a]
 -- filter takes a function and a list
  -- the function takes a value and returns a boolean
  -- the final return value is of the same type as the list passed into filter
-- map :: (a -> b) -> [a] -> [b]
 -- map takes a function and a list of type a
  -- the function passed in takes a value of type a and returns a value of type b
  -- the output of the map function is a list of type b

-- Q11.2
-- in Haskell, both tail and head have an error when called on an empty list. You can rewrite a version of tail that won't fail but instead return an empty list when called on an empty list. Can you write a version of head that returns an empty list when called on an empty list? To answer this, start by writing out the type signatures of both head and tail.
safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x:xs) = [x]

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- Q11.3
-- Recall myFoldl, what's the type signature of this funcitons? NOTE: foldl has a different type signature
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myfoldl f init (x:xs) = myFoldl f newInit xs
 where newInit = f init x






