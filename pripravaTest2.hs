


sumNote::[Int]->Int
sumNote[]=0
sumNote (first:rest) = first + sumNote rest

lenghtN::[Int]->Int
lenghtN[]=0
lenghtN (first:rest) = 1 + lenghtN rest

--average
--average::[Int]->Double
--average::[Int]->Int

 


average::[Int]->Int
average []=0
-- div 1 2  prefix
--  1  `div`  2 infix -- 1 (+) 2
average (list) = div (sumNote list) (lenghtN list) 
{--
average'::[Int]->Double
average' []=0
average' (list) =
    let x,f :: Int
    x = fromIntegral (sumNote list)
    f = fromIntegral (lenghtN list)
    in div x f

--}

    


removeThem::String->String
removeThem[]=[]
removeThem (first:rest) = 
    if first>='A' && first <='Z'
        then ['_'] ++ (removeThem rest)
    else
        [first] ++ (removeThem rest)

--['1''2''3'] = true
-- 1:[2,3]
ordered::[Int]->Bool
ordered [] = True
ordered [a] = True
ordered (first:second:rest) =
    if first <= second 
        then ordered(second:rest)
    else False

-- infix notace x+y
-- naze funk  par 1 par 2 to je infix // jinak je to prefi


--tuples jsou entice a musi se funkce definovat jako 

printUser::(Int,String,Int,Bool,String)->String
printUser (id,name,year,hasCard,sex) = "name is"++ name ++ " id is " ++ id ++ "year is" ++ year ++"hasCard"++ hasCard ++ "sex is" ++ sex 

-=======================================================