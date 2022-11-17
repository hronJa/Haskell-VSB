import Data.Ord

sumIt::[Int]->Int 
sumIt (x:xs) =
        if x `mod` 2 == 0  
            then x + sumIt xs  
        else  sumIt xs
--sumIt'::[Int]->Int

extract::[Char]->Int
extract [] = 0
extract (x:xs)
            | x >= 'a' && x <= 'z' = 1 + extract xs
            | otherwise = extract xs

countIt::Int->[Int]->Int
countIt n[] = 0
countIt n (x:xs) 
            | x == n = 1 + countIt n xs
            | otherwise = countIt n xs
palindrom::String->Bool
palindrom [] = False
palindrom st
        |st == reverse st = True
        | otherwise = False 

-- 1 a samosebou 1 neni 

max'::[Int]->[Int]
max' [] = []
max' input = [x| x <-input, x == findMax input]

append :: Int -> [Int] -> [Int]
append a xs = xs ++ [a]

findMax::[Int]->Int
findMax [x] = x 
findMax (x:xs) 
             | (findMax xs) > x = findMax xs
             | otherwise = x

--getThem[4,2,1] "Ahoj" =JoA

--getThem::[Int]->String->String
--getThem []_ = ""
--getThem _[a] = [a]
--getThem (x:xs) (s:ss)  
   --     | findLetter ss x /= "_" =  findLetter ss x ++ (getThem xs [s]) 
   --     | otherwise = getThem xs ss

--getThem (x:bs) (s:xs) = 

getH::[Int]->String->String
getH indexes string = map(string!!)indexes 
        
--[] ++ find rest 
findLetter::String->Int->String
findLetter (x:xs) n
                | (length xs) == n +1  = [x]
                | (length xs < n) || (length xs) < 0  = "_"
                | otherwise = findLetter xs n

getThem'::[Int]->String->String
getThem' (input:p) st = [x|x<-st,  findLetter st input /= "_"  ]
--getThem' (i:xs) st
 --      | length (take i st) /= (length st) = getThem' xs (take i st)
 --      | otherwise =getThem' xs st


fib::Int->Int
fib n = fst (tmp n) where
        fibStep(a,b) = (b, a+b)
        tmp 0 = (0,1)
        tmp x = fibStep (tmp(x-1))

fi::Int->Int
fi n = fst(tmp n ) where
        fibStep (a,b) = fibStep(b, a+b)
        tmp 0 = (0,1)
        tmp x = fibStep (tmp(x-1))

sumIt2::[Int]->Int
sumIt2 [] = 0
sumIt2 (x:xs)
        | (mod x 2) == 0 = x + sumIt2 xs
        | otherwise = sumIt2 xs

--isPrimev::Int->Bool
--isPrimev n = factors n == [1,n]

--factorss::Int->[Int]
--factorss n = [x|x <- [1..n], mod n x == 0]

isPrime::Int->Bool
isPrime n= factors n == [1,n]
 

factors::Int->[Int]
-- [1..n] generuje seznam 
-- x | x <- postupne to prirazuje do promenny x jen pokud ta podminka je true pokud ne tak se to preskoci
factors n = [x|x<-[1..n],mod n x ==0]

witoutPrimes::[Int]->[Int]
witoutPrimes []=[]
witoutPrimes intig = [x|x <-intig, isPrime x == False]



evens::String->String
evens [] =[]
evens (x:xs) 
        | mod (length xs ) 2 == 0 = [x] ++ evens xs  
        | otherwise = evens xs

findMax3::[Int]->Int
findMax3 [a] = a
findMax3 (x:xs)
        | (findMax3 xs) > x = findMax3 xs
        | otherwise = x
findMin::[Int]->Int
findMin [a]= a
findMin (x:xs)
        | (findMin xs)< x = findMin xs
        | otherwise = x

interval::[Int]->(Int,Int)
interval intigers =  (findMax3 intigers, findMin intigers)



filter'::[(String,Int)]->Int->[String]
filter' input n = [fst st| st <-input , snd st == n ]

fg::[(String,Int)]->Int->[String]
fg input n = [fst st| st <-input, snd st == n ]
        
--walk::[(Char,Int)]->String
--walk [] = []
--walk ((x,y):rest) = ( walk rest) ++ printXH' y x  
{-
mostFrequent::[Int]->Int
mostFrequent (input:rest)
                | countNumber rest input
-}


countNumber::[Int]->Int->Int
countNumber (x:xs) n 
                | (filter (==n) xs) /=[] = length (filter (==n) xs) + 1
                | otherwise = countNumber  xs n


aver2::[Double]->Double
aver2 [] = 0.0
aver2 list  = do
    let sum = sumD list
    let len = lent list
    ( sum) /( len)

sumD::[Double]->Double
sumD [] = 0.0
sumD (x:rest)  = x + sumD rest 

lent::[Double]->Double
lent [] = 0.0
lent (x: xs) = 1 + lent xs