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


getThem::[Int]->String->String
-- getThem input st = [x|x <-st, (input /= []) = map (findLetter st) input ]

getThem (x:bs) (s:xs) = 
        let tmp =[]
        in
         tmp ++ (map (findLetter xs) bs) 

findLetter::[Char]->Int->Char
findLetter (x:xs) n
                |  (length xs) == n = x
                | otherwise = findLetter xs n