-- robots have three basic properties
 -- a name
 -- an attack strength
 -- a number of hit points
 -- this will be a tuple triple

-- constructor for robot
robot (name, atk, hp) = \message -> message (name, atk, hp)

-- accessor functions for each value
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

-- setter functions to set new values to each part of the robot
 -- these will return a new instance of the robot
 -- the original robot is still available unaltered 
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

-- print stats for a robot
 -- need to use show for the a and h values 
printRobot aRobot = aRobot (\(n,a,h) -> n ++ " Attack: " ++ (show a) ++ " HP: " ++ (show h))

-- damage function will subtract amount from hp in robot
damage aRobot atkDmg = aRobot (\(n,a,h) -> robot (n,a,h - atkDmg))

-- fight function to interact two robots 
 -- allows fight if attacking robot has HP > 10
 -- if thats true return the attacking robots attack point and use that 
 -- else attacker attacks with 0
fight attacker defender = damage defender attack
 where attack = if getHP attacker > 10
                then getAttack attacker
                else 0

-- functions to map and list out all robots on the field of play
getBattleFieldStats robots = map printRobot robots
