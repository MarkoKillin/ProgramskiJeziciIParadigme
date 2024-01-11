--Napisati funkciju koja uklanja poslednji element liste.
ukloniPoslednji [] = []
ukloniPoslednji [x] = []
ukloniPoslednji (x:xs) = x : ukloniPoslednji xs

--Napisati funkciju koja uklanja pretposlednji element liste.
ukloniPretposlednji [] = []
ukloniPretposlednji [x] = [x]
ukloniPretposlednji [x,y] = [y]
ukloniPretposlednji (x:xs) = x : ukloniPretposlednji xs

--Izracunati n! rekurzivno
faktoriel 0 = 1
faktoriel 1 = 1
faktoriel n = n * faktoriel (n-1)

--Izracunati n! repno rekurzivno
faktoriel' n = fakt 1 n
fakt acc 0 = acc
fakt acc n = fakt (n*acc) (n-1)

--Napisati funkciju "imaVelikaSlova" koja proverava da li neki string ima velika slova
imaVelikaSlova [] = False
imaVelikaSlova (x:xs) 
    | x >= 'A' && x <= 'Z' = True
    | otherwise = imaVelikaSlova xs

--Napisati funkciju "spljosti". [1, 2, 2, 2, 3, 3, 4, 5, 5, 5] -> [1, 2, 3, 4, 5]
spljosti [] = []
spljosti [x] = [x]
spljosti (x:y:ys) 
    | x == y = spljosti (y:ys)
    | otherwise = x : spljosti (y:ys)

--Napisati funkciju koja iz liste stringova izbacuje one koji imaju sva mala slova
izbaciSaMalim [] = []
izbaciSaMalim (x:xs) 
    | imaVelikaSlova x = x : izbaciSaMalim xs
    | otherwise = izbaciSaMalim xs

--Napisati funkciju koja kvadrira sve elemente liste (preko ZF izraza).
kvadriraj x = [s*s | s <- x]

--Napisati funkciju jeDeljiv, koji prima 2 broja i vraca true ako prvi broj moze da deli drugi.
jeDeljiv x y
    | mod y x == 0 = True
    | otherwise = False

--Napisati funkciju jeDeljivSa3 koristeci prethodno definisanu funkciju
jeDeljivSa3 x = jeDeljiv 3 x

--Napisati funkciju filter' f l, koja filtrira elemente liste l pomocu funkcije f.
filter' f [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

--Definisati funkciju deljiviSa3 koristeci prethodne 2 funkcije.
deljiviSa3 [] = []
deljiviSa3 l = filter' jeDeljivSa3 l