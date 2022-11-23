data DynamicTree a = Leaf a | Branch [DynamicTree a] deriving(Show)

treeExample :: DynamicTree Int
treeExample = Branch [Leaf 1, Branch [Leaf 1, Leaf 3]]

-------------
data FileType = Image | Executable | SourceCode | TextFile deriving (Eq, Show)

data Entry = File {nam :: String, size :: Int, ftype :: FileType}
           | Directory {nam :: String, entries :: [Entry]} deriving (Eq, Show)

root :: Entry
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

countFiles :: Entry -> Int
countFiles (File {}) = 1
countFiles (Directory _ files) = sum (map countFiles files)

----------------------------------------------------------

fullNames :: Entry -> [String]
fullNames entry = getNames [entry] ""
  where
    getPath p1 p2 = p1 ++ "/" ++ p2
    getNames [] _ = []
    getNames ((File name _ _) : es) path = getPath path name : getNames es path
    getNames ((Directory name files) : es) path = getNames files (getPath path name) ++ getNames es path