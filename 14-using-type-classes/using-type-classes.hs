import Data.List

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

instance Show SixSidedDie where 
 show S1 = "I"
 show S2 = "II"
 show S3 = "III"
 show S4 = "IV"
 show S5 = "V"
 show S6 = "VI"

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where 
 compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1, f1) (l2,f2)

names :: [Name]
names = [Name ("Joe", "Shmoe"),
         Name ("Jill", "Mill"),
         Name ("Johnny", "Nonny")]

-- Q14.1
-- Note that Enum doesn't require either Ord or Eq even though it maps types to Int values which implements both Eq and Ord. Ignoring the fact that you can easily use deriving for Eq and Ord, use the derived implementation of Enum to make manually definint the Eq and Ord much easier.
data Numbers = One | Two | Three deriving (Enum)

instance Ord Numbers where
 compare n1 n2 = compare (fromEnum n1) (fromEnum n2)

instance Eq Numbers where
 (==) n1 n2 = (fromEnum n1) == (fromEnum n2)

-- Q14.2
-- Define a five-sided Die (FiveSidedDie Type). Then define a type class name Die and at lieast one method that would be useful to have for a die. Also include super-classes you think make sense for a die. Finally make you FiveSidedDie and instance of Die.

data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
 roll :: Int -> a

instance Die FiveSidedDie where
 roll n = toEnum (n `mod` 5)