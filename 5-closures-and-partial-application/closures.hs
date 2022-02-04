genIfXEven :: Integral p => (p -> p) -> p -> p
genIfXEven f =  (\x -> ifEven f x)

ifEven :: Integral p => (p -> p) -> p -> p
ifEven f x = if even x 
             then f x 
             else x

getRequestURL :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestURL host apiKey resource id = host ++
                                        "/" ++ 
                                        resource ++
                                        "/" ++
                                        id ++
                                        "/" ++
                                        apiKey

genHostRequestBuilder :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostRequestBuilder host = (\apiKey resource id ->
                               getRequestURL host apiKey resource id)

exampleUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilder = genHostRequestBuilder "http://example.com"
                                                                      
genApiRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiRequestBuilder hostBuilder apiKey = (\resource id ->
                                            hostBuilder apiKey resource id)

genApiResourceBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiResourceBuilder hostBuilder apiKey resource = (\id -> 
                                                      hostBuilder apiKey resource id)                                            

exampleUrlBuilder2 :: [Char] -> [Char] -> [Char]
exampleUrlBuilder2 = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

exampleUrlBuilder3 :: [Char] -> [Char]
exampleUrlBuilder3 = genApiResourceBuilder exampleUrlBuilder "1337hAsk3ll" "book"

exampleUrlBuilder4 :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilder4 = getRequestURL "http://example.com" 

exampleUrlBuilder5 :: [Char] -> [Char] -> [Char]
exampleUrlBuilder5 = exampleUrlBuilder4 "1337hAsk3ll"

exampleUrlBuilder6 :: [Char] -> [Char]
exampleUrlBuilder6 = exampleUrlBuilder3 

subtract2 :: Integer -> Integer
subtract2 = flip (-) 2

-- Q5.1 
-- Now that you know about partial application, you no longer need to use genIfEvenX. Redefine ifEvenInc, ifEvenDouble, and ifEvenSquare by using ifEven and partial application.
inc :: Integer -> Integer
inc n = n+1

dbl :: Integer -> Integer
dbl n = n*2

sqr :: Integer -> Integer
sqr n = n^2

ifEvenInc :: Integer -> Integer
ifEvenInc = ifEven inc

ifEvenDbl :: Integer -> Integer
ifEvenDbl = ifEven dbl

ifEvenSqr :: Integer -> Integer
ifEvenSqr = ifEven sqr

-- Q5.2
-- Even if Haskell didn't have partial application, you could hack together some approximations. Following a similar pattern to flipBinaryArgs, write a function binaryParialApplicaiton that takes a binary function and one argument and returns a new function waiting for the missing argument.

binaryPartialApplication binFunc arg = binFunc arg