--binarni strom, nejjednodusi list je cislo a slozitejsi jsou operace a ty tvori uzly




data Expr = Num Int 
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        | Var Expr Expr  -- prom pro derivaci
    deriving(Eq)


-- 1+ 2 konstatni funkce
-- priorita je urcena ne zavorku ale uzly ..jakym zpusoben seradime uzly ve strome
-- 
test1::Expr
test1 = Add (Num 1)(Num 2)

-- 1 +2 * 3
test2::Expr
test2 = Add(Num 1) (Mul (Num 1) (Num 3))

--(1+2) * 3
test3::Expr
test3 = Mul (Add (Num 1) (Num 2)) (Num 3)


-- 2x * (x-1)
--test4:: Mul( Mul (Num 2)(Var 'x')) (Sub(Var 'x')(Num 1))



-- Circle Rectangle jsou konstruktory a jsou taky funkce !!
data Shape = Circle Float Float Float | Rectangle Float Float Float

data Person = Person String String Int Float String String deriving (Show)

eval::Expr->Int
eval (Num x ) = x
eval (Add x y) =  eval x + eval y
eval (Sub x y) =  eval x - eval y
eval (Mul x y) =  eval x * eval y
eval (Div x y) =   eval x + eval y


firstName::Person->String
firstName (Person firstName _ _ _ _ _) = firstName

lastName::Person->String
lastName (Person _ lastName _ _ _ _) = lastName

age::Person->Int
age (Person _ _ age _ _ _) = age

height::Person->Float
height (Person _ _ _  height _ _) = height

phoneNumber::Person->String
phoneNumber (Person _ _ _ _ phoneNumber _) =phoneNumber

flavor::Person->String
flavor (Person _ _ _ _ _ flavor) = flavor


--- dinamicky strom 

