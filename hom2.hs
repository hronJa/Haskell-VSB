import Debug.Trace

--pp(drawTriangles  (40,15) [(Triangle (Point 7 4) (Point 33 4) (Point 20 14)),(Triangle (Point 7 11) (Point 33 11) (Point 20 1))])

data Point = Point Int Int
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
    
processTriangleList::Array->[Triangle]->Result
processTriangleList array [] = array
processTriangleList array (first:rest) =  processTriangleList (drawTriangle array first) rest

type Array = [[Char]]

allocate::Int->Int->Array
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

replaceRow::Array->Int->[Char]->Array 
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


main = do
    pp(drawTriangles  (40,15) [(Triangle (Point 7 4) (Point 33 4) (Point 20 14)),(Triangle (Point 7 11) (Point 33 11) (Point 20 1))])
    --drawTriangles  (40,15) [(Triangle (Point 7 4) (Point 33 4) (Point 20 14)),(Triangle (Point 7 11) (Point 33 11) (Point 20 1))]
