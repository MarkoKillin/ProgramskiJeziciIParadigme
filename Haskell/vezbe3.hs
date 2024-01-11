--1. Napisati funkciju koja prihvata String i razdvoji ga
--   po nekom karakteru. 
--   rastaviString :: Char -> String -> [String]
--  primer: "ana voli milovana" -> ["ana", "voli", "milovana"]
razdvoji c [] = []
razdvoji c l = razdvojiAcc c [] l
razdvojiAcc c acc [] = [acc]
razdvojiAcc c acc (x:xs)
    | x == c = acc : razdvojiAcc c [] xs
    | otherwise = razdvojiAcc c (acc ++ [x]) xs

--2. Napisati funkciju koja prihvata listu Stringova.
--   Sve stringove spoji tako sto izmedju svaka 2 
--   umetne karakter ','.
-- ["ana", "voli", "milovana"] + ',' -> "ana,voli,milovana"
spoji [] = []
spoji l = spojiAcc [] l
spojiAcc acc [] = acc
spojiAcc acc (x:xs) 
    | length xs > 0 = spojiAcc (acc ++ x ++ ",") xs
    | otherwise = spojiAcc (acc ++ x) xs

--3. Napisati funkciju koja privhata listu Stringova, 
--   razdvoji svaki po razmaku, te ih sve
--   spoji zarezima. Koristiti map, fold i prethodne 2 funkcije.
-- ["ana voli milovana", "trava je zelena"]
-- -> ["ana", "voli", "milovana", "tr ava", "je", "zelena"]
-- -> "ana,voli,milovana,trava,je,zelena"
funkcija3 l = spoji (foldr (++) [] (map (razdvoji ' ') l))

--4. Napisati funkciju koja prihvata listu listi integer-a ( [[Int]] ).
--   Potrebno je prvo kvadrirati elemente svake podliste, zatim ih sumirati.
--   Na kraju potrebno je vratiti proizvod te liste. (map, fold)
--  svastaSaListom :: Num a => [[a]] -> a
--  [[1, -4, 5], [4, 4, 4], [-4, -6, -2]] -> 112896
funkcija4 l = foldr (*) 1 sum
    where kvad = map (\x -> map (\y -> y*y) x) l
          sum = map (\x -> foldr (+) 0 x) kvad  

--5. Napisati funkciju koja prihvata 2 liste: listu radnika [String] i listu 
--   njihovih ranih brojeva [Int]. Ako liste nisu iste duzine vratiti praznu listu
--   kao error, u suprotnom spojiti obe liste u listu parova (koristiti zip), a zatim
--   izbaciti sve radnike koji imaju neparan radni broj.
funkcija5 l1 l2
    | length l1 /= length l2 = []
    | otherwise = filter (\(x,y) -> mod y 2 == 0) spoj
        where spoj = zip l1 l2

--6. Funkcija ocekuje 3 parametra: centralnu tacku C u 2D prostoru (Int, Int), listu drugih 
--   tacaka [(Int, Int)], i distanca D (double). Treba iz liste tacaka izbaciti sve tacke
--   koje su udaljene vise od D od centralne tacke C.
