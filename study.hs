{-
Define following functions that performs corresponding logic operations: not', and', or', nand', xor', impl', equ'
Define the 'standard' priority for all these functions, if they are used as operators.
Create a function that prints the truth table of a given logical expression for two variables.
-}

funNOT::Bool->Bool
-- pattern matching
-- potom a je vstupni hodnota a haskell zna jeji typ funNOT a 
funNOT  a
    | a == True = False
    | otherwise = True 
-- prave ze je to pattern matching muzeme to napsat tim to zpusobem
funNOT'::Bool->Bool
funNOT' True = False
funNOT' False = True

funOR::Bool->Bool->Bool
funOR a b 
    | a /= False || b /= False = True
    | otherwise = False
funAND::Bool->Bool->Bool
funAND a b 
    | a == True && b == True = True
    -- | a == b = a
    | otherwise = False

-- generovany seznam generujeme seznam bool  
-- neberu nic  a vystup je bool seznam
-- (b->b->b) je pattern 
genList::(Bool->Bool->Bool)->[Bool]
-- _ bude vstup ale nepouzije se
-- op je funkce ktera se ma pouzit
-- do x prom a y prom se budou vkladat hodnoty  ted true and false
genList op = [ op x y|x <-[True,False],y <-[True,False]]

-- definice table kulaty zavorky  tup.
-- () je to eintice  Bool
-- (b,b,b) je bin operace bere 2 vraci jeden
table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
-- generovany seznam trojic  x,y a vysledek op
table op = [(x,y, op x y)|x <-[True,False],y <-[True,False]]

 