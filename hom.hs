--[('u',5),('r',5),('d',5),('l',10),('d',5),('r',5),('u',5)]



printXH::Int->String->String
printXH 0 _ = []
printXH n input = input ++ (printXH (n-1) input)

printXH'::Int->Char->String
printXH' 0 _ = []
printXH' n input = [input] ++ (printXH' (n-1) input)


printStep::Int->Char->String
printStep 0 _ = []
printStep n input 
            | input == 'l' || input == 'r' = "x" ++ "\n" ++ (printStep (n -1) input)
            | otherwise = "x" ++ (printXH' (n-1) 'x')

--walkTurtle::[(Char,Int)]->String
---
countContent::Int->Int->Int
countContent a b = a * b

walkTurtleW::[(Char,Int)]->String
walkTurtleW  [] = []
--[st|st<- walkTurtle rest, st printXH' y x ]
walkTurtleW ((x,y):rest) = [st|st<-printXH' y x ]

walk::[(Char,Int)]->String
walk [] = []
walk ((x,y):rest) = ( walk rest) ++ printXH' y x  

walkWitDirection::[(Char,Int)]->String
walkWitDirection [] = []
walkWitDirection ((x,y):rest)
        | x == 'l' || x == 'r' =  (walkWitDirection rest) ++ printStep y x 
        | otherwise  = ( walkWitDirection rest) ++ printStep y x  
         
{--
max'::[(Char,Int)]->Int->Int
max' [] _ = 0
max' _ inp = inp
max' ((x,y):rest) inp
     | (x == 'l' || x == 'r') && y > inp = max' rest y

--}



    
pp::Result->IO()
pp x = putStr(concat(map(++"\n")x))
sampleInput::Result
sampleInput ="
"     XXXXX"
"     X   X"
"     X   X"
"     X   X"
"XXXXXXXXXX"
"X   X     "
"X   X     "
"X   X     "
"XXXXX     "



