-- declaration of fucntionis
sayHi :: IO()
-- this is a function but never used
sayHi = putStrLn "Hello users!"

sayName :: String -> IO()
sayName name = putStrLn ("Hello " ++ name) 
main :: IO()
main = do
    putStrLn "Firs"
    sayHi
    sayName "Jana"
    putStrLn "Last"