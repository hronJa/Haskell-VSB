import Debug.Trace

type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

draw::[(Char,Int)]->Result 
draw input = do
  let dimensions = calculateDimensions input
  let cols =  ((dimensions !! 0) + (dimensions !! 2))
  let rows =  ((dimensions !! 1) + (dimensions !! 3))
  
  let x0 = div cols 2
  let y0 = div rows 2
  let array = allocate rows cols
  let origin = setAt (array!!y0) x0 '+'
  let withOrigin = replaceRow array y0 origin

  let updated = markAllPaths withOrigin x0 y0 input
  let skipPrefix =  foldr min cols ( map getSpacePrefixLength updated)
  let skipSuffix =  foldr min cols ( map getSpacePrefixLength (map reverse updated))
 -- print skip
  --show skip)
 -- printResult updated skipPrefix skipSuffix
  let withoutEmptyRows = removeEmptyRows updated
  map (removeEmptyCols skipPrefix skipSuffix) withoutEmptyRows
  --  print printResult updated skipPrefix skipSuffix

markAllPaths::Array->Int->Int->[(Char,Int)]->Array
markAllPaths array x y [] = array
markAllPaths array x y (first:rest) = do
  let direction = getFirstElementFromTuple first 
  let nextDirection = if (length rest) > 0 then getFirstElementFromTuple ((take 1 rest)!!0) else '_'
  let length = (getSecondElementFromTuple first)
  let newStartPoint = getNewStartPoint x y direction nextDirection length
  let newX = newStartPoint!!0
  let newY = newStartPoint!!1
  markAllPaths (markPath array x y direction length) newX newY rest
 

getNewStartPoint::Int->Int->Char->Char->Int->[Int]
getNewStartPoint x y direction nextDirection length = do
  let correction = if direction == nextDirection then 0 else 1 

  if direction == 'u' then [x,y-length+correction]
  else if direction == 'd' then [x,y+length-correction]
  else if direction == 'l' then [x-length+correction,y]
  else [x+length-correction,y]


markPath::Array->Int->Int->Char->Int->Array
markPath array x y direction 0 = array
markPath array x y direction length = do 
  let updated = setAt (array!!y) x 'X'
  let replaced = replaceRow array y updated
  let newLength = length - 1
  if direction == 'u' then markPath replaced (x) (y-1) direction newLength
  else if direction == 'd' then markPath replaced (x) (y+1) direction newLength
  else if direction == 'l' then markPath replaced (x-1) (y) direction newLength
  else markPath replaced (x+1) (y) direction newLength

replaceRow::Array->Int->[Char]->Array 
replaceRow array index newRow = 
  let split = splitAt index array in
  (getFirstElementFromTuple split) ++ [newRow] ++ (tail (getSecondElementFromTuple split))

getDirectionPath::Char->(Char,Int)->Int
getDirectionPath requiredDirection (direction, step)
            | direction == requiredDirection = step
            | otherwise = 1
        

processInput::[(Char,Int)]->Char->Int
processInput input dir =  sum (map (getDirectionPath dir ) input)

calculateDimensions::[(Char,Int)]->[Int]
calculateDimensions input = map (processInput input) ['l','u','r','d']

getSecondElementFromTuple::(Ord a, Ord b)=>(a,b)->b 
getSecondElementFromTuple (a,b) = b

getFirstElementFromTuple::(Ord a, Ord b)=>(a,b)->a 
getFirstElementFromTuple (a,b) = a

getSpacePrefixLength::[Char]->Int
getSpacePrefixLength [] = 0
getSpacePrefixLength (first:rest) 
                    | first == ' ' = 1 + getSpacePrefixLength rest
                    | otherwise = 0


type Array = [[Char]]

setAt::[Char]->Int->Char->[Char]
setAt input index newValue = 
  let split = splitAt index input in 
  (getFirstElementFromTuple split) ++ [newValue] ++ (tail (getSecondElementFromTuple split))

allocate::Int->Int->Array
allocate 1 cols = [(generateRow cols)]
allocate rows cols = (allocate (rows - 1) cols) ++ [(generateRow cols)]

generateRow::Int->[Char]
generateRow 0 = []
generateRow length = [' '] ++ (generateRow (length - 1))


removeEmptyRows::[[Char]]->Result
removeEmptyRows input = [n| n<-input, (elem 'X' n) ]

removeEmptyCols::Int->Int->[Char]->[Char]
removeEmptyCols skipPrefix skipSuffix input = do 
  let withoutPrefix = drop skipPrefix input
  reverse (drop skipSuffix (reverse withoutPrefix))
 
-- testing
-- pp(draw [('u',5),('r',5),('d',5),('l',10),('d',5),('r',5),('u',5)]) 