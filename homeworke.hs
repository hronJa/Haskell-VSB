{-

pole = arr_alloc_and_zero(N);

*Main> pp (draw [('u',5),('r',5),('d',5),('l',10),('d',5),('r',5),('u',5)])      

-}

--draw::[(Char,Int)]->String

type Result = [String]
--pp :: Result -> IO ()
--pp x = putStr (concat (map (++"\n") x))

--calculateDimensions::[(Char,Int)]->(Int,Int)
--calculateDimensions [] = (0,0)
--calculateDimensions  (x:xs) 

getDirectionPath::Char->(Char,Int)->Int
getDirectionPath requiredDirection (direction, step)
            | direction == requiredDirection = step
            | otherwise = 0

processInput::[(Char,Int)]->Char->Int
processInput input dir =  sum (map (getDirectionPath dir ) input)

calculateDimensions::[(Char,Int)]->[Int]
calculateDimensions input = map (processInput input) ['u','d','r','l']

getAt::[Int]->Int->Int
-- split at vraci  seznam seznamu, last vezme posledni seznam
getAt arrInput index = head ( getLastElementFromTuple (splitAt index arrInput))

getLastElementFromTuple::(a,b)->b 
getLastElementFromTuple (a,b) = b

getFirstElementFromTuple::(a,b)->a 
getFirstElementFromTuple (a,b) = a

setAt::[Int]->Int->Int->[Int]
setAt input index newValue = (getFirstElementFromTuple (splitAt index input)) ++ [newValue] ++ (tail (getLastElementFromTuple (splitAt index input)))