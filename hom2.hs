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
  --  let x = drawPoint array a
  --  let y = drawPoint x b
  --  drawPoint y c 
--drawTriangle::Result->Triangle->String
--drawTriangle array triangle = "item"
      let x = drawLine array a b 
      let y = drawLine x b c 
      drawLine y c a

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
drawLine array p1 p2 = drawLinePoints array (generatePoints p1 p2) 

drawLinePoints::Result->[Point]->Result
drawLinePoints array [] = array
drawLinePoints array (x:xs) = drawLinePoints (drawPoint array x) xs  

generatePoints::Point->Point->[Point]
generatePoints (Point x1 y1) (Point x2 y2) = do
  -- abs absolutni hodnota
  let sideA = abs (x2 - x1)
  --let ratioSides =  abs (div (y2 - y1) (x2 - x1))
  let sideB = abs (y2 - y1)
  -- 0.5 * 100 = 50 
  let ratio =  ( fromIntegral (sideB)) / (fromIntegral (sideA))
  let rationS = round (100 * ratio)
  -- pokud x2 > x1  tak multiplier +1 jinak -1 a to same pro y
  let xMultiplier  
                  | x2 > x1 = 1 
                  | otherwise = -1 
  let yMultiplier  
                  | y2 > y1 = 1 
                  | otherwise = -1 
   
  -- x * ratioSides  = y // nova hodnota  ale chci absolutni hodnotu  proto + y1
  -- A [3,5] B[5,5]
  -- x 0,1,2 
  -- 3 + 1 * 0 = 3
  -- 3 +1 *1 = 4
  -- 3 +1 *2 = 5
{-
A [5,5] B[3,5] 3-5 =-2
  -- x 0,1,2 
  -- 5 -1 *0 = 5
  -- 5 -1 *1 = 4
  -- 5 -1 *2 = 3

  ! posledni bod y nefunkcin pro  5 5 , 2 3
-}

  [Point (x1 + xMultiplier * x) (y1+ yMultiplier * (min sideB (  (div (x * rationS) 100) )))  | x <-[0..sideA]]



{-

Point1 x1,y1 Point2 x2,y2

count = x2 - x1
ratioSides = (y2 - y1) / (x2 - x1) // hodnota  napr 2 10/5

for x in 1..count  //  pokud bude count 5 tak je to od  1 do 5 probehene 5x
    y = x * ratioSides
    drawPoint x y  
-}



main = do
  
    --print (map show ())
    pp(drawTriangles  (40,15) [(Triangle (Point 7 4) (Point 33 4) (Point 20 14)),(Triangle (Point 7 11) (Point 33 11) (Point 20 1))])
   -- pp (drawTriangles  (40,15) [(Triangle (Point 10 0) (Point 0 5) (Point 15 5))])
