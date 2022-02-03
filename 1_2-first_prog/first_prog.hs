main :: IO()
main = do
 print "Who is the email for?"
 recipient <- getLine 
 print "What is the Title"
 title <- getLine 
 print "Who is the Author"
 author <- getLine 
 print (createEmail recipient title author)

toPart :: [Char] -> [Char]
toPart recipient = "Hello " ++ recipient ++ ",\n"

bodyPart :: [Char] -> [Char]
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ". I hope you enjoyed it!\n"

fromPart :: [Char] -> [Char]
fromPart author = "Warmest Regards,\n" ++ author

createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recipient bookTitle author = toPart recipient ++ 
                                         bodyPart bookTitle ++ 
                                         fromPart author

