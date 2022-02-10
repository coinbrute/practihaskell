import Data.List (sort)
import Data.Semigroup
-- Composability or the compination of two like things is an important concept to grasp with these two things (semigroups and monoids)

-- what is it? 
 -- concatenating two lists to get one new list 
 -- combine two documents to get a new document
 -- mix two colors to get a new color

-- examples of composability with combining functions 
-- this is important to grasp the concept of this 
-- remember the (.) operator is just function composition infix right precedance (9) 

-- get the last elem in a list
myLast :: [a] -> a
myLast = head . reverse

-- get min elem in a list
myMin :: Ord a => [a] -> a
myMin = head . sort

-- get max elem in a list
myMax :: Ord a => [a] -> a
myMax = myLast . sort

-- test that prop is true against all elem in a list
myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)
-- *Main> myAll (>3) [4,5,5,6,7,8]
-- True

-- QC17.1
-- Implement myAny by using function composition. myAny tests tat a property is True for at least one value in a list
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

-- Semigroup type class. 
-- the main method used is (<>) used to combine instances of the same type. 
-- here you can implement Semigroup for Integer types by defining <> as +
instance Semigroup Integer where 
 (<>) x y = x + y

-- here is the type signature for (<>)
-- (<>) :: Semigroup a => a -> a -> a 

-- QC17.2
-- can you use (/) to make Int a Semigroup?
 -- no since (/) doesn't always return an int. but you could with `div`
instance Semigroup Int where
 (<>) x y = x `div` y

-- working with colors and combining colors as an example of semigroups and combining like types.
data Color = Red
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown
           | Clear deriving (Show, Eq)

instance Semigroup Color where
 (<>) Clear  any    = any
 (<>) any    Clear  = any
 (<>) Red    Blue   = Purple
 (<>) Blue   Red    = Purple
 (<>) Yellow Blue   = Green
 (<>) Blue   Yellow = Green
 (<>) Yellow Red    = Orange
 (<>) Red    Yellow = Orange
 (<>) a      b      | a == b = a
                    | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
                    | all (`elem` [Blue,Yellow,Green]) [a,b] = Green | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange | otherwise = Brown
-- now by using composition and semigroups with the color Data type above combining colors is possible.

-- many times there are laws to be followed with nontrivial type classes and associativity is one of them. 
-- associativity is the relationships from combinations of combinations


-- QC17.3 
-- Does the above implementation of Semigroup of Integers support associativity? 
-- yes as (+) for Integers already is associative
-- 1+2+3

-- similar to the Semigroup is Monoid

-- the primary difference is Monoid requires an identity element for the type

-- identity element means that 
 -- x <> id = x (and id <> x = x)
 -- so for Integers the identy element would be 0.
 -- Colors above dont have an identity element. 

-- definition of Monoid class
-- class Monoid a where
--  mempty :: a
--  mappend :: a -> a -> a
--  mconcat :: [a] -> a

-- mconcat takes a list of Monoids and combines them returning one Monoid

-- mappend mempty x is x 
 -- since mappend is the same as (++)  and mempty is [] for list this means 
  -- [] ++ [1,2,3] = [1.2.3]

-- second rule is first with reverse order args
 -- [1,2,3] ++ [] = [1.2.3]

-- third is mappend x (mappend y z) = mappend (mappend x y) z
 -- with lists this is easiest to see
  -- [1] ++ ([2] ++ [3]) = ([1] ++ [2]) ++ [3]

-- fourth is the mconcat definition
mconcat' :: Monoid a => [a] -> a
mconcat' = foldr mappend mempty

----------------------------------------------------------------


-- take a list of strings representing events and a list of Doubles for probabilities 
-- this will represent a coin toss event 

data Events = Events [String]
data Probs  = Probs [Double]

-- make a table to compile list of events pairs with probabilities
data PTable = PTable Events Probs

-- now we need a create function for PTable. 
 -- basic constructor that ensures probabilities all sum to 1
createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
 where totalProbs      = sum probs
       normalizedProbs = map (\x -> x/totalProbs) probs

-- we will need to make PTable an instance of Show as well but here is a function to print a single event/probability pair
showPair :: String -> Double -> String
showPair event prob = mconcat' [event, "|", show prob, "\n"]

instance Show PTable where
 show (PTable (Events events) (Probs probs)) = mconcat pairs
  where pairs = zipWith showPair events probs

-- now we need to begin to think about a way to turn this into a Monoid and begin working with it. 
-- This starts with being able to generate a combination of all events and all probabilities i.e the Cartesian product.
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func xs ys = zipWith func newXs cycledYs
 where nToAdd     = length ys -- need to repeat each elem in the first list once for each elem in the second
       repeatedXs = map (take nToAdd . repeat) xs -- map xs and makes nToAdd copes of the element
       newXs      = mconcat' repeatedXs -- last line outputs a list of lists here you join them
       cycledYs   = cycle ys -- by cycling the second list you can zipWith to combine the two lists

-- now we can have functiosn for combining events and probabilities as specific cases
instance Semigroup Events where 
 (<>) = combineEvents

instance Monoid Events where
 mempty = Events []
 mappend = (<>)

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
 where combiner = (\x y -> mconcat [x, "-", y]) -- hyphenate event names to combine them

instance Semigroup Probs where
 (<>) = combineProbs

instance Monoid Probs where
 mempty = Probs []
 mappend = (<>)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2) -- multiply probabilities to combine them

-- At this stage PTable can become an instance of Semigroup
instance Semigroup PTable where
 (<>) ptable1        (PTable (Events []) (Probs [])) = ptable1
 (<>) (PTable (Events []) (Probs [])) ptable2        = ptable2
 (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
  where newEvents = combineEvents e1 e2
        newProbs = combineProbs p1 p2

-- notice above what has happened. We implemented mappend through the use of (<>) and also gave PTable an mempty value since we defined a PTable [] [] case

-- now we can make PTable an instance of Monoid
 -- remember mconcat comes along for the ride
instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

-- here are some examples of different PTables
coin :: PTable 
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"])  (Probs [0.1, 0.2, 0.7])

-------------------------------------------------

-- Q 17.1
-- the current implementation of Color doesn't contain an identity element. Modify the code in this unit so that Colr does have an identity element, and then make Color an instance of Monoid.
-- see above for addition of identity.
 -- added a new base color Clear to represent lack of color or emptiness and compared to any returning any did the variation like above with PTable in the Semigroup instance


-- Monoid instance definition below
 -- define the mempty as the Clear Color type
instance Monoid Color where
 mempty = Clear
 mappend = (<>)

-- Q 17.2 
-- if your Events and Probs types were data types and not just synonyms, you could make them instances of Semigroup and Monoid, where combineEvents and combineProbs were the <> operator in each case. Refactor these types and make instances of Semigroup and Monoid.
 -- above I made several strict type dedclarations to Events and Probs now that each are of type Monoid and Semigroup.
 -- all functionality is still the same more or less.