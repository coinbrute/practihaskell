-- product types are created by combining two or more existing types with 'and' 

-- a fraction can be defined as a numberator (Integer) and denominator (Integer)

-- a street address may be a number (int) and a street name (String

-- a mailing address may be a street address and a city (String) and a state (String) and a zip (Int)

-- think struct in Solidity or C
-- Class in Java combining types of objects etc
{-

  In C

  struct author_name {
  char *first_name;
  char *last_name;
  };
  struct book {
  author_name author;
  char *isbn;
  char *title;
  int year_published;
  double price;
  };

  In Haskell
  data AuthorName = AuthorName String String
  data Book = Autohr String String Int Double
-}

-- though record syntax is easier to use. 
data AuthorName = AuthorName {
 firstName  :: String
 , lastName :: String
}

data Book = Book {
 author       :: Creator
 , isbn       :: String
 , bookTitle  :: String
 , bookYear   :: Int
 , bookPrice  :: Double
}

data VinylRecord = VinylRecord {
 artist :: Creator
 , recordTitle :: String 
 , recordYear :: Int
 , recordPrice :: Double
}

-- Quickcheck 16.2
-- assuming you have a Car type. How could you represent a SportsCar as a Car with a Spoiler assuming you have a Spoiler type as well.
data Car = Car String
data Spoiler = Spoiler String
data SportsCar = SportsCar Car Spoiler

-- you can also create types with the or flag. 
-- think boolean it is either a True or False type

data Bool = False | True

type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName 
 | NameWithMiddle FirstName MiddleName LastName
 | TwoInitialsWithLast Char Char LastName
 | FirstNameWithTwoInits FirstName Char Char

data Creator = AuthorCreator Author | ArtistCreator Artist

instance Show Creator where 
 show a = show a

data Author = Author Name

data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

data CollectibleToy = CollectibleToy {
 name :: String
 , description :: String
 , toyPrice :: Double
}

price :: StoreItem -> Double 
price (PamphletItem pamphlet) = 0.00
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "Unknown"


-- Q16.1
-- to further complicate the items in your store, you eventually keep an inventory of free pamphlets. pamphlets have a title a description adn a contact field for the organization that provides the pamphlet. Create the Pamphlet type and add it to StoreItem. Additionally modify the price so that it works with Pamphlet
data Pamphlet = Pamphlet {
 title :: String
 , pamphletDesc :: String
 , contact :: String
}

-- Q16.2
-- Create a Shape type that includes the following shapes: Circle Square Rectangle. Then write a function to compute the perimeter of a Shape as well as its area.
type Radius = Float
type Height = Float
type Width  = Float

data Shape = Circle Radius | Square Height | Rectangle Height Width deriving Show

perimeter :: Shape -> Float
perimeter (Circle r)      = 2 * pi * r
perimeter (Square h)      = 4 * h
perimeter (Rectangle h w) = 2*h + 2*w

area :: Shape -> Float
area (Circle r)       = pi * r^2
area (Square h)      = h^2
area (Rectangle h w) = h * w