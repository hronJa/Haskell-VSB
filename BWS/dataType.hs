
data Color a = Black
             | White
             | RGB a a a

--funkce ktere manipuluji s datovzm typem 

isBlack::Color Int->Bool
isBlack Black = True
isBlack (RGB x y z) = x == 0 && y == 0 && z == 0
isBlack _ = False

-- datovy konstruktor muze obssa 


data FileType = 
          Image
        | Executable
        | SourceCode
        | TextFile
    
data Entry = File { name::String, size::Int, ftype::FileType}
        |Directory {name::String, entries::[Entry]}
        
root::Entry
root = Directory "root"
    [
        File "logo.jpg" 5000 Image,
        Directory "classes"
            [
                File "notes-fpr.txt" 200 TextFile,
                File "presentation.jpg" 150 Image,
                File "first_test.hs" 20 SourceCode
            ]
    ]

count::Entry->Int
count (File name size ftype) = 1
count (Directory name entries) = 
        