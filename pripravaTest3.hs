take'::Int->[a]->[a]
take' _[] =[]
take' 0 _ =[]
take' n (first:rest) = first: take'(n-1) rest

drop'::Int->[a]->a 
drop' _[] =[]
drop' 0 x= x
drop' n (first:last) = drop' (n-1) xs

min::Ord a=>[a]->a 
min [x] = x 
min(x:y:z) | x<y = min(x:z)
           | otherwise min(y:z)

