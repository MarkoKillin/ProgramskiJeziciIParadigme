
--Napisati funkciju "ispeglaj" koja prima listu ciji su elementi
--liste [[Int]], a vraca listu brojeva [Int] tako sto sve elemente
--podlisti spoji u jednu listu.
-- [[1, 2, 3], [4, 5], [6], [7, 8], [], [9]] -> [1, 2, 3, 4, 5, 6, 7, 8, 9] 
ispeglaj :: [[Int]] -> [Int]
ispeglaj [] = []
ispeglaj (x:xs) = x ++ ispeglaj xs

--Napisati funkciju koja prima listu listi brojeva [[Int]], i vraca
--listu suma elemenata listi koje ona sadrzi. Koristiti funkciju fold.
-- [[1, 2, 3], [2, 3], [1, 2, 4], [5]] -> [6, 5, 7, 5]
subsum :: [[Int]] -> [Int]
subsum [] = []
subsum (x:xs) = (foldr (+) 0 x) : subsum xs

--Napisati funkciju koja prima listu ciji su elementi liste brojeva,
--te iz svake podliste izbacuje parne brojeve. Ako slucajno ostane
-- neka prazna lista, nju je potrebno izbaciti. Koristiti funkciju filter.
-- [[1, 2, 3], [2, 4], [3, 4, 5], [7]] -> [[1, 3], [3, 5], [7]]
izbaciParne :: [[Int]] -> [[Int]]
izbaciParne [] = []
izbaciParne l = filter (\x -> length x > 0) (map (filter (\x -> mod x 2 == 1)) l)

-- Napisati funkciju "okreni" koja prolazi kroz listu stringova, 
-- te pomocu funkcija map i reverse "okrene" sve stringove.
-- "neki string" -> "gnirts iken"
okreni :: [String] -> [String]
okreni [] = []
okreni l = map (reverse) l

-- Napisati funkciju koja prihvata listu listi brojeva [[Int]] i 
-- iz svake podliste izbacuje elemente deljive sa 3.
izbaciDeljive :: [[Int]] -> [[Int]]
izbaciDeljive [] = []
izbaciDeljive l = map (filter (\y -> mod y 3 /= 0)) l

-- Napisati funkciju koja prihvata listu listi brojeva, primenjuje na 
-- nju prethodnu funkciju i onda izbaci sve podliste koje imaju manje od 5 elemenata.
izbaci5 :: [[Int]] -> [[Int]]
izbaci5 [] = []
izbaci5 l = filter (\x -> length x > 5) (izbaciDeljive l)