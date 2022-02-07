-- need to be able to show and work within GHCi
-- enumerate in a list and easily convert to an Int. This allows for simple math to rotate out letters 
 -- fromEnum to convert letters to ints and toEnum to convert ints to letters
-- and keep bounded by for minBound and maxbound values to know how far to cycle

{-
 Algorithm functionality: 

 - pass in the size of alphabet and a letter to rotate off of 
 - use the div function to find the middle.
 - to rotate you add half of your alphabet size to the int value of your letter. 
 - Mod your offset by the alphabet size as the added half wil give an int value larger than the maxbound value
 - finally use toEnum to convert this int representation of your letter back into an instance of the letters type
-}
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded) 

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphSize c = toEnum rotation
 where middle   = alphSize `div` 2
       offset   = fromEnum c + middle
       rotation = offset `mod` alphSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)
-- Side Note Enum values start at 0 so its generally safe to assume that the total number of items in any alphabet is maxBound + 1

rotChar :: Char -> Char
rotChar charToEncrypt = rotN alphSize charToEncrypt
 where alphSize = 1 + fromEnum (maxBound :: Char)

fourLetterAlphEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet] 
fourLetterAlphEncoder vals = map rot4l vals
 where alphSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
       rot4l    = rotN alphSize

fourLetterAlphDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet] 
fourLetterAlphDecoder vals = map rot4l vals
 where alphSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
       rot4l    = rotNDecoder alphSize

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]


-- the issue with odd sized alphabets
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMsg :: [ThreeLetterAlphabet]
threeLetterMsg = [Alpha,Alpha,Beta,Alpha,Kappa]

-- this will decode incorrectly
-- threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
-- threeLetterEncoder vals = map rot3l vals
--  where alphSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
--        rot3l    = rotN alphSize

-- this will encode correctly
threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
 where alphSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3l    = rotN alphSize

-- this will decode correctly
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3l vals
 where alphSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3l    = rotNDecoder alphSize

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder size c = toEnum rotation
 where middle   = size `div` 2
       offset   = if even middle
                  then fromEnum c + middle
                  else 1 + fromEnum c + middle
       rotation = offset `mod` size

-- now with the decoder principles above we can encode and decode strings and individual characters
rotEncoder :: String -> String
rotEncoder text = map rotChar text
 where size    = 1 + fromEnum (maxBound :: Char)
       rotChar = rotN size

rotDecoder :: String -> String
rotDecoder text = map rotChar text
 where size    = 1 + fromEnum (maxBound :: Char)
       rotChar = rotNDecoder size




-- what is xor
-- true when one is true 
-- else false
{-
first val           second value               result
false               false                      false
true                false                      true
false               true                       true
true                true                       false
-}

{-
Example with binary

10110       10001
xor         xor
00111       10110
_____       _____
10001       00111

-}

-- xorBool the foundation of xor
xorBool :: Bool -> Bool -> Bool
xorBool val1 val2 = (val1 || val2) && (not (val1 && val2))
-- val1 = true val2 = false 
-- (true | false) && (not (true && false))
-- true && not(false)
-- true && true
-- true

-- val1 = true val2 = true
-- (true | true) && (not (true && true))
-- true && not(true)
-- true && false
-- false

-- ideally we want to be able to work with pairs so we can zip two lists together then map values across the list of pairs.
-- here is the xorPair function
xorPair :: (Bool, Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2

-- now we can use this to do operations on lists of Bools
xor :: [Bool] -> [Bool] -> [Bool]
xor bs1 bs2 = map xorPair (zip bs1 bs2)

-- now that we can zip up lists of boolean values after xor'ing the values we need to figure out how to do it on strings
-- first lets create a useful type synonym Bits
type Bits = [Bool]

-- we can start by converting Ints to bits since each Char can be converted to an Int. 
-- then we need to transfor a base 10 into a stream of bits i.e. the binary equivalent
-- we do this by recursively dividing a number by 2 
 -- if there is no remainder then add False (0) to the list of bits otherwise add (1) 
 -- stop when the number is either 1 or 0
-- here is a intsToBits function to do just that
-- ensure we reverse the final output of the algorithm 
-- also to ensure the bit lists are the same size make sure to prepend extra False vaues so that the list is equal to the size of the length of the int converted to bits.
-- we need a maxBits value
maxBits :: Int
maxBits = length (intsToBits' maxBound)

intsToBits' :: Int -> Bits
intsToBits' 0 = [False]
intsToBits' 1 = [True]
intsToBits' n = if (remainder == 0)
                then False : intsToBits' nextVal
                else True : intsToBits' nextVal
 where remainder = n `mod` 2
       nextVal   = n `div` 2

intsToBits :: Int -> Bits
intsToBits n = leadingFalses ++ reversedBits
 where reversedBits  = reverse (intsToBits' n)
       missingBits   = maxBits - (length reversedBits)
       leadingFalses = take missingBits (cycle [False])

-- now we can convert chars to bits
charsToBits :: Char -> Bits
charsToBits char = intsToBits (fromEnum char)

-- then we need a way to convert those chars to bits back to chars and ints to bits and bits to ints
bitsToInts :: Bits -> Int
bitsToInts bits = sum (map (\x -> 2^(snd x)) trueLocations)
 where size          = length bits
       indices       = [size - 1, size - 2 .. 0]
       trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChars :: Bits -> Char
bitsToChars bits = toEnum (bitsToInts bits)


-- lets use the code below to test the xor cypher
myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plainTextBits)
 where padBits        = map charsToBits pad 
       plainTextBits = map charsToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChars bitList
 where bitList = applyOTP' pad plainText

-- using partial application to create an encoder/decoder
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

-- now lets create a class for capturing the general behaviour of encoding and decoding to use across ciphers that may be created.
class Cipher a where 
 encode :: a -> String -> String
 decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where 
 encode Rot text = rotEncoder text
 decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
 encode (OTP pad) text = applyOTP pad text
 decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

