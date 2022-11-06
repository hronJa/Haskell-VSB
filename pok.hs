--fact 0 = 1
--f[] =
-- constantni seznamy f[1,2,3] = je stejne jako  f (1:2:3)
--f [x] =
   -- f[x,y] =
        --f(x:y:[]) jak x a y jsou 
        --f(x:xs) x je nove cislo a xs je cely ten seznam
        -- length(_:xs) = 1+ .. skript

sumIt :: [Int] -> Int
-- spatna prce se seznamem
-- tmp je nazev funkce where je nadrazenost
--sumIt xs = tmp 0 0 where xs
--    tmp i suma leng | i == leng = suma
--                    | otherwise = tmp(i+1) (suma + xs !! i) leng

- dokazuje ze indexovani v haskl ju zbytecne a vede ke spatnemum vysledku
sumIt xs = tmp xs 0 where
    tmp [] suma = suma
    tmp (x:xs) suma = tmp xs (suma + x)

    -- jak vytvorim fc ktera vrati jen 
