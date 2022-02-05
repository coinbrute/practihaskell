-- model for a cup of coffee
-- cup can have one property
 -- then umber of onces of liquid currently in it

-- cup constructor
 -- by passing in a message we can create a cup object 
cup flOz = \message -> message flOz

-- this is a getter to retrieve the state of a cup "object"
getOz aCup = aCup (\flOz -> flOz)

-- this is a state change function to alter the cup object
 -- check that oz in cup - oz drank is GT 0 
 -- if it is then drink the oz passed in
 -- else cup will be zero oz left
drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
 where flOz = getOz aCup
       ozDiff = flOz - ozDrank

-- check whether cup is empty
isEmpty aCup = getOz aCup == 0