import Data.Char
-- Definisati funkciju koja prima listu brojeva i ukoliko je
-- ona parne duzine, svaki element kvadrira, inace svaki element
-- mnozi sa 10. Koristiti funkciju map i lambda funkcije.
funkcija1 [] = []
funkcija1 l
    | mod (length l) 2 == 0 = map (\x -> x*x) l
    | otherwise = map (\x -> x*10) l

-- Napisati funkciju koja prima niz karaktera. Prvo je potrebno
-- funkcijom filter izbaciti sva velika slova, a zatim pomocu funkcije
-- map pretvoriti sva preostala slova u velika.
-- Napomena: Potrebno je u .hs fajl importovati funkciju toUpper
-- iz modula Data.Char, na sledeci nacin:
-- import Data.Char -- Dodati na pocetak fajla
funkcija2 :: [Char] -> [Char]
funkcija2 l = map (toUpper) (filter (\x -> (x >= 'a' && x <= 'z')) l)

-- Napisati funkciju "primeni" koja prima 3 parametra:
--	1) f1 :: [[Int]] -> [[Int]] - funkciju koja prihvata [[Int]] i vraca [[Int]]
--	2) f2 :: [[Int]] -> [Int] - funkciju koja prihvata [[Int]] i vraca [Int]
--	3) listu ciji su elementi liste brojeva, tj. [[Int]]
--	Funkcija treba da primeni funkcije f1 i f2 nad prosledjenom listom
--	i vrati rezultat (listu tipa [[Int]] pretvara u [Int], pomocu 
--	funkcija f1 i f2).
primeni f1 f2 l = f2 $ f1 l

-- Definisati funkciju "izbaciParnePaSumiraj" preko funkcije "primeni"
--   i funkcija iz zadataka 2 i 3 (sa Vezbi 2), koristeci parcijalnu 
-- primenu funkcija.

-- Definisati funkciju prosecnaDuzina koja prima niz stringova
-- i racuna njihovu prosecnu duzinu pomocu funkcije fold i map.
-- Dovoljno je vratiti celobrojnu vrednost (npr. 5 umesto 5.3)
prosecnaDuzina l = div suma (length l)
    where duzine = map length l
          suma = foldr (+) 0 duzine