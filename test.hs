-- 1 funkce 
-- "ABCDEF" = BDF
odds::String->String
odds [] = []
odds (x:xs) 
        | mod (length xs ) 2 == 1 = [x] ++ odds [xs] 
        | otherwise = odds xs


accounts::[(String,Int)]->[String]
accounts input = [fst st| st<-input, snd st > 0]

countS::String->Char->Int
countS (x:xs) s
    | (filter (==s) xs) /=[] = length (filter (==s) xs) + 1
    | otherwise = countS xs s 

check::String->Bool
check [] = False
check input 
    | (countS input '(' /= countS input ')') = False
    | otherwise = True