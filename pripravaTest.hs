doubleMe::Int->Int
doubleMe x = x + x

doubleUs::Int->Int->Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallerNumber::Int->Int
doubleSmallerNumber x = if x > 100
                            then x
                            else x*2
                    

 

-- strings hello are just lists of charakters == ['h','e','l','l','o']
--function to get length of string

getLength::[a]->Int
getLength [] = 0
getLength (_:xs) = 1 + getLength xs

{-
isPrime::Int->Bool
isPrime 1 = True
isPrime n = isPrimeTest n(n-1) where
            isPrimeTest n 1 = True
            isPrimeTest n i | n mod i == 0 = False
                            | otherwise = isPrime n (i-1)
-}

sayHi::(Integral a)=>a->String
sayHi 1 = "One"
sayHi 2 = "Two"
sayHi 3 = "Three"
sayHi 4 = "Four"
sayHi x = "x number"
--sumIt 1:[2,3]
--sumIt 1(a):(b)[2,3]
sumIt::[Int]->Int
sumIt[] = 0
sumIt(a:b) = a + sumIt b

--['H',"l",'l','o'] []+1 1[1]
getLen::[a]->Int
getLen[] = 0
getLen(x:s) = 1 + getLen s
 
{-

Relevant bindings include
        s :: [a] (bound at pripravaTest.hs:46:10)
        x :: a (bound at pripravaTest.hs:46:8)
        getLen :: [a] -> Int (bound at pripravaTest.hs:45:1)

 let (x:xs) = "hello"
 "hello" == 'h' : ['e','l','l','o']
True
ghci> x
'h'
ghci> xs
"ello"

-}
-- 1:[2,3,4]
-- 2:[]
-- return int !!
-- 1:[2]
sumEvenNumber::[Int]->Int
sumEvenNumber []=0
-- rest is []
sumEvenNumber (first:rest)= 
    if first `mod` 2 ==0
        -- 2+0 // 2+[] =[2]
       then first + sumEvenNumber rest
    else sumEvenNumber rest

extract::[Char]->Int
extract []=0
extract (first:rest)= 
    if first >='a' && first <'z'
        then 1 + extract rest
    else extract rest

-- input 9 and [1,2,2,5,9,9] = 2
-- input 1 and [1] = 1
-- input 1 and [1,2] = 1
-- input 1 and [1,1] = 2
countIt::Int->[Int]->Int
-- libovolny parametr 9 a pak seznam []
countIt searchNumber [] = 0
countIt searchNumber (first:rest)= 
    if first == searchNumber 
        then 1 + countIt searchNumber rest
    else countIt searchNumber rest


sumPossitive::[Int]->Int
sumPossitive [] = 0

sumPossitive (first:rest)=
    if(first >=0)
        then first + sumPossitive rest
    else sumPossitive rest

-- input "ABCCBA" output true
-- input "ANNA" output true
-- input "A" output true
-- input "AN" output false
palindrom::String->Bool
palindrom input= input == reverseString input

-- "ahoj" joha
-- a hoj
-- ? + a
-- (h oj) + a
-- ((o j) + h) + a
-- (j) + o + h + a
-- [] + j + o + h + a
reverseString::String->String
reverseString []=[]
-- vztup je sezname a typ je char a zbytek coz je list  proto []
reverseString (first:rest)= (reverseString rest) ++ [first] 


-- reverse se da napsat jinak 
-- 1,2,3,4,7
-- 2
isPrime::Int->Bool
isPrime n= factors n == [1,n]
 

factors::Int->[Int]
-- [1..n] generuje seznam 
-- x | x <- postupne to prirazuje do promenny x jen pokud ta podminka je true pokud ne tak se to preskoci
factors n = [x|x<-[1..n],mod n x ==0]

-- ['1','2'] 'Ahoj' ah
--getThem::[Int]->String->String

--lehkz

