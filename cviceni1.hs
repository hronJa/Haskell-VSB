factorial :: Int->Int
factorial 0=1
factorial n = n* factorial(n-1)

fib :: Int->Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibVer2 :: Int -> Int
fibVer2 n = tmp n 1 1 where
  tmp 0 a _ = a 
  tmp x a b = tmp (x-1) b (a+b) 

  {-
  
  bool isPrime(int n){
    int i = n-1;
    while(i>1){
        if(n%i==0){
            return false;
        }
        i--;
    }
    return true;
  }
  
  -}

  isPrime :: Int -> Bool
  --isPrime 1 = True
  isPrime n = isPrimeTest n(n-1) where
              isPrimeTest n 1 = true
              isPrimeTest n i | n mod i == 0 = False
                            | otherwise = isPrime n (i-1)