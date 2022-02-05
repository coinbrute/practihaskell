-- you use type synonyms to referr to the same type by two names 
-- think [Char] and String
type FirstName = String
type LastName = String
type MiddleName = String
type Age = Int
type Height = Int

data Name = Name FirstName LastName 
            | NameWithMiddle FirstName MiddleName LastName
-- type construcor 
 --       the sex type is an instance of either of these data construcors
data Sex = Male | Female
--      The data constructors can be used just like values i.e. True False
-- Essentially Sex can either be Male or Female

data RhType = Pos | Neg
data ABOType = A | B | AB | O
-- data constructor for blootype states that it takes a ABOType and a RHType
data BloodType = BloodType ABOType RhType
-- record syntax allows for a psuedo json type structure and allows for auto getters to be created behind the scene
data Patient = Patient { name :: Name,
                         sex :: Sex,
                         age :: Int,
                         height :: Int,
                         weight :: Int, 
                         bloodType :: BloodType }

janeDoe :: Patient
janeDoe = Patient { name = Name "Jane" "Doe",
                    sex = Female,
                    age = 27,
                    height = 65,
                    weight = 127,
                    bloodType = BloodType AB Neg }

johnDoe :: Patient
johnDoe = Patient { name = Name "John" "Doe",
                    sex = Male,
                    age = 31,
                    height = 70,
                    weight = 145,
                    bloodType = BloodType O Pos }

patientInfo :: Name -> Age -> Height -> String
patientInfo (Name f l) age height = name ++ " " ++ ageHeight
 where name = f ++ ", " ++ l 
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

firstName :: Name -> FirstName
firstName (Name f _) = f
firstName (NameWithMiddle f _ _) = f

lastName :: Name -> LastName
lastName (Name _ l) = l
lastName (NameWithMiddle _ _ l) = l

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
-- takes a BloodType so we need to use the consturctor here and contruct a BloodType by passing in both the things needed to construct one before deconstructing it in the function body
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- rules for blood donation
 -- A can donate to A and AB
 -- B can donate to B and AB
 -- AB can donate only to AB
 -- O can donate to anybody
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _                = True
canDonateTo _               (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _)  = True
canDonateTo (BloodType B _) (BloodType B _)  = True
canDonateTo _               _                = False

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType 
getBloodType (Patient _ _ _ _ _ bt) = bt

-- Q12.1 
-- write a function similar to canDonateTo that takes two patients as args rather than two BloodTypes
isDonor :: Patient -> Patient -> Bool
isDonor patient1 patient2 = canDonateTo (bloodType patient1) (bloodType patient2)

-- Q12.2
-- implement a patienSummary function that uses you final Patient type. patientSummary should output a string that looks like this:
{-
**************
Patient Name: Smith, John
Sex: Male
Age: 46
Height: 72 in.
Weight: 210 lbs.
Blood Type: AB+
**************
-}
patientSummary :: Patient -> String
patientSummary patient = "**************\n" ++ 
                         "Patient Name: " ++ showName (name patient) ++ "\n" ++ 
                         "Sex: " ++ showSex (sex patient) ++ "\n" ++
                         "Age: " ++ show (age patient) ++ "\n" ++
                         "Height: " ++ show (height patient) ++ " in.\n" ++
                         "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
                         "Blood Type: " ++  showBloodType (bloodType patient) ++ 
                         "**************"