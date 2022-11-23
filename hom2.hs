import Debug.Trace

--pp(drawTriangles  (40,15) [(Triangle (Point 7 4) (Point 33 4) (Point 20 14)),(Triangle (Point 7 11) (Point 33 11) (Point 20 1))])

data Point = Point Int Int deriving (Show)
data Triangle = Triangle Point Point Point
type Result = [String]


pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

drawTriangles :: (Int,Int)-> [Triangle]->Result
drawTriangles (cols,rows) triangleList = do
    let array = allocate rows cols
    processTriangleList array triangleList
    
    -- na jednotlive indexy sposti printTriangle
   -- map (drawTriangle array) triangleList
    
processTriangleList::Result->[Triangle]->Result
processTriangleList array [] = array
processTriangleList array (first:rest) =  processTriangleList (drawTriangle array first) rest



allocate::Int->Int->Result
allocate 1 cols = [(generateRow cols)]
allocate rows cols = (allocate (rows - 1) cols) ++ [(generateRow cols)]

generateRow::Int->[Char]
generateRow 0 = []
generateRow length = ['.'] ++ (generateRow (length - 1))

drawTriangle::Result->Triangle->Result
-- zkusit pres map
--drawTriangle array (Point x1 y1 ,Point x2 y2,Point x3 y3) =
drawTriangle array (Triangle a b c) = do
    let x = drawPoint array a
    let y = drawPoint x b
    drawPoint y c 
--drawTriangle::Result->Triangle->String
--drawTriangle array triangle = "item"

drawPoint::Result->Point->Result
drawPoint array (Point x y) = do
    let newRow = setAt (array!!y) x '#'
    replaceRow array y newRow

replaceRow::Result->Int->[Char]->Result 
replaceRow array index newRow = 
  let split = splitAt index array in
  (getFirstElementFromTuple split) ++ [newRow] ++ (tail (getSecondElementFromTuple split))

setAt::[Char]->Int->Char->[Char]
setAt input index newValue = 
  let split = splitAt index input in 
  (getFirstElementFromTuple split) ++ [newValue] ++ (tail (getSecondElementFromTuple split))


getSecondElementFromTuple::(Ord a, Ord b)=>(a,b)->b 
getSecondElementFromTuple (a,b) = b

getFirstElementFromTuple::(Ord a, Ord b)=>(a,b)->a 
getFirstElementFromTuple (a,b) = a

drawLine::Result->Point->Point->Result
drawLine array (Point x1 y1) (Point x2 y2) = array


generatePoints::Point->Point->[Point]
generatePoints (Point x1 y1) (Point x2 y2) = do
  -- abs absolutni hodnota
  let sideA = abs (x2 - x1)
  --let ratioSides =  abs (div (y2 - y1) (x2 - x1))
  let sideB = (y2 - y1)
  let ratio = sideB / sideA
  let ratioSides =  abs (round ratio) 
  //dodelat  deleni 
  [Point (x+x1) ((x * ratioSides)+y1)  | x <-[0..sideA]]



{-

Point1 x1,y1 Point2 x2,y2

count = x2 - x1
ratioSides = (y2 - y1) / (x2 - x1) // hodnota  napr 2 10/5

for x in 1..count  //  pokud bude count 5 tak je to od  1 do 5 probehene 5x
    y = x * ratioSides
    drawPoint x y  
-}



main = do
  print "Hello"
    --print (map show ())
  --  pp(drawTriangles  (40,15) [(Triangle (Point 7 4) (Point 33 4) (Point 20 14)),(Triangle (Point 7 11) (Point 33 11) (Point 20 1))])
    --drawTriangles  (40,15) [(Triangle (Point 7 4) (Point 33 4) (Point 20 14)),(Triangle (Point 7 11) (Point 33 11) (Point 20 1))]
