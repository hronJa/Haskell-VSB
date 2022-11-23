-- Zadani 2
-- Ukol1
data Entity = Point {x :: Double, y :: Double} | Circle {x :: Double, y :: Double, r :: Int} | Contrainer [Entity]

-- UkolZadani2,3
data Article = Text String | Section String [Article] deriving (Show)

myArticle :: Article
myArticle = Section "Document" [
    Section "Introduction" [
        Text "My introduction",
        Section "Notation" [Text "alpha beta gamma"]],
    Section "Methods" [
        Section "Functional Programming" [Text "FPR"],
        Section "Logical Programming" [Text "LPR"]],
    Section "Results" [Text "All is great"]]

-- Ukol2 (vrati seznam bloku textu obsazenych v objektech typu Text)
allTexts :: Article -> [String]
allTexts (Text str) = [str]
allTexts (Section _ list) = concatMap allTexts list

-- Ukol3 (vrati seznam kapitol, ktere ve svem seznamu typu [Article] neobsahuji Text)
names :: Article -> [String]
names Text {} = []
names (Section name list) | check list = name : concatMap names list
                          | otherwise = concatMap names list 
                            where
                                check :: [Article] -> Bool
                                check = foldr ((&&) . ch) True 
                                    where
                                        ch Text {} = False
                                        ch Section {} = True

-- Zadani 1
-- ukol1
data Company = Company {name :: String, employees :: Int, ownerOf :: [Company]}

myCompany :: Company
myCompany = Company "Spolecnost" 15000 [Company "Spolecnost2" 50 []]

-- Ukol2 (vrati seznam nazvu kapitol)
allSections :: Article -> [String]
allSections Text {} = []
allSections (Section name list) = name : concatMap allSections list

-- Ukol3 (vrati nejvyssi pocet vnoreni)
articleDepth :: Article -> Int 
articleDepth Text {} = 0
articleDepth (Section _ list) = 1 + maximum (map articleDepth list)


-- dalsi na procviceni
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

-- vrátí seznam dvojic: (jméno adresáře, velikost adresáře)
directorySizes :: Entry -> [(String, Int)]
directorySizes File {} = []
directorySizes (Directory name files) = (name, sum [countSize x | x <- files]) : concat [directorySizes x | x <- files]

countSize :: Entry -> Int
countSize (File _ size _) = size
countSize (Directory _ files) = sum (map countSize files)


-- dostane jako první parametr název souboru a z adresářové struktury, předané jako druhý parametr, odstraní všechny výskyty souboru s tímto jménem
removeFile :: String -> Entry -> Entry
removeFile str (File name size ftype) = File name size ftype
removeFile str (Directory name files) = Directory name (check str [removeFile str x | x <- files])

check :: String -> [Entry] -> [Entry]
check _ [] = []
check str ((Directory name files):xs) = Directory name files : check str xs
check str ((File name size ftype):xs) | name == str = check str xs
                                      | otherwise = File name size ftype : check str xs