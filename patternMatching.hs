-- pattern matching consists of specifying patterns to which some data should  conform and then checking to see if it does
   -- lucky :: (Integral a) => a -> String
   -- lucky 7 = "Lucky number SEVEN"
   -- lucky x = "sorry you are out of luck, pal"

    factorial 0 = 1
    factorial 1 = 1
    factorial n = n * factorial (n-1)