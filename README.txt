#GHC is an Interpretor
modules in Haskell
- are composed from functions and user defined data types

special package imported by default Prelude
#hiding functions



prikaz

ghci 
:l load
:r reload
#functions
f x
f x y
f (g x)
-- singel line comments
{-
multiple line comments
-}

-- learnyouahaskell.com 
--Imports modules 

start script to load
:l nameOfscript
:r read output of the variable

-- to print multiple output function is do


test 21.10
vyraz zda je spravne
definice typu funkce
f::a->a->a
g::Int->Int
napsat jednoduchou funkci, test papier

07.10
///// opakovani 
staticky typovane , staticky silne typovane, 

o cem je typovaci system realne bude ten program pracovat s daty
a taky si mohu vytvaret nove "moje datove typy"

jak naspat funkci  s pattern matching a pote mam dalsi konstrukce 
- pattern matching napisu tu funkci jako soubor 
constata a promenna

rekurzivni datove struktury

pokud chci ulozit array Haskell nema ma seznam / list
je homoge. datova strukuta jen string nebo jen int  atd

co koliv pracuje s listem jsou tam [] [1,2,3 ...]
data a List a = Cons a (List a) | Nil

jak se to muze psat "sugar" List a  je stesjny jako [a]

list ()

Jaky typ bude funkce
max:: [a]-> a 
data color = Black | white |Red 

do priste naspat funkce jak pracovat se seznamem





Prelude> doubleMe = map (2*)
Prelude> xs = [1,2,3,4,5,6,7,8]
Prelude> doubleMe xs
[2,4,6,8,10,12,14,16]
