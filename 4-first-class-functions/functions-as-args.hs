import Data.List ( sort, splitAt )
import Data.Text.Lazy (splitOn)

ifEven :: Integral p => (p -> p) -> p -> p
ifEven f n = if even n 
             then f n
             else n

inc :: Num a => a -> a
inc n = n+1

double :: Num a => a -> a
double n = n*2

square :: Num a => a -> a
square n = n^2

neg :: Num a => a -> a
neg n = (-n)

cube :: Num a => a -> a
cube n = n^3

ifEvenInc :: Integral p => p -> p
ifEvenInc = ifEven inc 

ifEvenDouble :: Integral p => p -> p
ifEvenDouble = ifEven double 

ifEvenSquare :: Integral p => p -> p
ifEvenSquare = ifEven square 

ifEvenNeg :: Integral p => p -> p
ifEvenNeg = ifEven neg 

ifEvenCube :: Integral p => p -> p 
ifEvenCube = ifEven cube 

ifEvenCubeLambda :: Integral p => p -> p
ifEvenCubeLambda = ifEven (\x -> x^3) 

names :: [([Char], [Char])]
names = [("Ian", "Curtis"),
         ("Bernard","Summer"),
         ("Peter","Hook"),
         ("Stephen","Morris"),
         ("Roger","Morris")]

sorted :: Ord a => [a] -> [a]
-- sorted xs = sort xs
sorted = sort

compareLastNames :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
compareLastNames name1 name2 = if lastNames == EQ 
                               then compare (fst name1) (fst name2)
                               else lastNames
 where lastNames = compare (snd name1) (snd name2)

-- using guards instead
compareLastNamesWGuards :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
compareLastNamesWGuards name1 name2 
                            | lastName1 > lastName2   = GT
                            | lastName2 > lastName1   = LT 
                            | firstName1 > firstName2 = GT
                            | firstName2 > firstName1 = LT
                            | otherwise               = EQ 
 where lastName1  = snd name1
       lastName2  = snd name2
       firstName1 = fst name1
       firstName2 = fst name2

addressLetterV1 :: ([Char], [Char]) -> [Char] -> [Char]
addressLetterV1 name location = nameText ++ " - " ++ location
 where nameText = fst name ++ " " ++ snd name

sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = if head lastName < 'L'
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
 where lastName = snd name 
       nameText = fst name ++ " " ++ lastName

nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
 where nameText = fst name ++ " " ++ snd name

renoOffice :: ([Char], [Char]) -> [Char]
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
 where nameText = snd name

dcOffice :: ([Char],[Char]) -> [Char]
dcOffice name = nameText ++ " Esq."
 where nameText = fst name ++ " " ++ snd name

getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location = case location of
                               "ny"   -> nyOffice
                               "sf"   -> sfOffice
                               "reno" -> renoOffice
                               "dc"   -> dcOffice
                               _      -> (\name -> fst name ++ " " ++ snd name)

addressLetterV2 :: ([Char], [Char]) -> [Char] -> [Char]
addressLetterV2 name location = locationFunction name
 where locationFunction = getLocationFunction location                               

-- mapName xs = (head (splitOn "\s" xs), last (splitOn "\s" xs))

-- main = do
--  print ("What is your name?")
--  name <- getLine 
--  mapName name

 
