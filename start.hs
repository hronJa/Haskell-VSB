--import Data.list
--Types
-- bool, int, intiger, float, doublem charm tuples

trueAndFalse = True && False

maxInt = maxBound::Int

---MATH
addNum = 3+6
subNum = 3-6
modNum = mod 9 2
-- meaning that it maps froma  string to a string

removeNonUpprecase::[String]->[String]
removeNonUpprecase st =[c|c <- st, c 'elem'['A'..'Z']]

-- parameter are separated with -> and there is no spacial distinction between the parameters and the return Types
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z