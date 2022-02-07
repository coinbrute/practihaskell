-- :t (+)
-- (+) :: Num a => a -> a -> a 

-- use :info to inspect a class 

-- defining a type class 

{-
     name of type class 
             type variable as a placeholder for the sprific type that will implement this class 
class TypeName a where 
 names of all the required functions
   type signatures of the required functions
 fun1 :: a -> a
 fun2 :: a -> String
 fun3 :: a -> a -> Bool

-}

class Describable a where 
 describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- Q13.1
-- If you ran the :info examples, you likely noticed that the type Word has come up a few times. Without looking at external resources, use :info to explore Word and the relevant type classes to come up with your own explanation for the Word type. How is it different from Int?
{-
 They have the same type class definition and are of the same instances. 
 The difference would be in their Bounds.
-}

-- Q13.2
-- One type class we didn't discuss is Enum. Use :info to look at the definition of this type class, as well as example members. Now consider Int, which is an instance of both Enum and Bounded. given the following definition of inc: 
{- 
inc :: Int -> Int
inc x = x + 1
-}
-- and the succ function required by Enum, what's the difference between inc and succ for Int?
-- succ will fail because it is bounded and inc will roll over

-- Q13.3
-- Write the following function thatworks just like succ on Bounded type but cna be called an unlimited number of times without error. The function will work like inc in the preceding example but works on a wider range of types, including types that aren't members of Num:
-- Definition will include functions/values from Bounded,Enum, and the mystery type class. Make a note of where each of these three functions/values come from.
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n
