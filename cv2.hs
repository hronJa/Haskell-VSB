-- oddList
-- 2 inputs 1 output
oddList::Int->Int->[Int]
oddList a b = [n | n <-[a..b], n `mod` 2 == 1 ]

-- The Eq class defines equality (==) and inequality (/=).
-- All the basic datatypes exported by the Prelude are instances of Eq,
-- and Eq may be derived
-- for any datatype whose constituents are also instances of Eq. 
union::Eq a => [a]->[a]->[a]
union as bs = as ++ [ b | b <- bs, not (elem b as)]

intersection::Eq a => [a]->[a]->[a]
intersection as bs =  [ b | b <-bs, (elem b as) == True ]

--countThem::String->[(Char,Int)]

-- h a dostanu 3
countSameLetter::String->Char->Int
countSameLetter []_ = 0
countSameLetter (x:xs) a 
                | x==a = 1 + countSameLetter xs a
                | otherwise = countSameLetter xs a


countThem::String->[(Char,Int)]
countThem [] = []
countThem st = [(x,y)| x<-unique st, y<-[countSameLetter st x]]

unique::String->String
unique [] = []
unique (x:xs) = x: unique(filter (/=x) xs)


tuplesFirst::(Char,Int)->Char
tuplesFirst (x,_) = x

tuplesSecond::(Char,Int)->Int
tuplesSecond (_,i) = i 

printXH::Int->String->String
printXH 0 _ = []
printXH n input = input ++ (printXH (n-1) input)

printXH'::Int->Char->String
printXH' 0 _ = []
printXH' n input = [input] ++ (printXH' (n-1) input)