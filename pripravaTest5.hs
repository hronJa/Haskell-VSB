sumIts::[Int]->Int
sumIts [] = 0
sumIts (first:rest) = 
    if first `mod` 2 ==0
        then first + sumIts rest 
    else sumIts rest

extract::[Char]->Int 
extract [] = 0
extract (first:rest) =
    if first>'a' && first <'z'
        then 1 + extract rest
    else extract rest

mSum::[Int]->Int
mSum []=0
mSum (x:xs) = x + mSum xs

length'::[Int]->Int
length'[]=0
length' (x:xs) = 1 + length' xs

aver::[Int]->Double
aver [] = 0.0
aver list = do
    let sum = mSum list
    let len = length' list
    (fromIntegral sum) /(fromIntegral len)

main = do
    let list = [1,2,3]
    print(aver list)

countIn::Int->[Int]->Int
countIn searchNumber []=0
countIn searchNumber (first:rest) = 
    if first == searchNumber
        then 1 + countIn rest 
    else countIn searchNumber rest

sumPos::[Int]->Int
sumPos numb [] = 0
sumPos numb (first:rest) =
    if first > 0
        then first + sumPos rest
    else sumPos rest