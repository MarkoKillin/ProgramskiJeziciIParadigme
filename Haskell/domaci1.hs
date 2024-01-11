--Napisati funkciju koja uklanja svaki N-ti element liste.
ukloniNti n [] = []
ukloniNti n l = ukloniNtiAcc 1 n l
ukloniNtiAcc acc n [] = []
ukloniNtiAcc acc n (x:xs) 
    | mod acc n == 0 = ukloniNtiAcc (acc + 1) n xs
    | otherwise = x : ukloniNtiAcc (acc + 1) n xs

--Napisati funkciju koja vraca listu delilaca nekog pozitivnog broja.
delioci 0 = [0]
delioci 1 = [1]
delioci n = deliociAcc n 1 []
deliociAcc n del l
    | n == del = l
    | mod n del == 0 = del : deliociAcc n (del + 1) l
    | otherwise = deliociAcc n (del + 1) l

--Napisati funkciju koja sumira sve elemente liste.
sumiraj :: [Int] -> Int
sumiraj [] = 0
sumiraj [x] = x
sumiraj (x:xs) = x + sumiraj xs
sumirajReduce l = foldr (+) 0 l 

--Napisati funkciju zip' koja prima 2 liste i sabira elemente na istim indeksima. 
zip' [] [] = [0]
zip' [x] [y] = [x + y]
zip' l [] = l
zip' [] l = l
zip' (x:xs) (y:ys) = (x + y) : zip' xs ys

--Napisati funkciju koja sumira sve cifre prosledjenog broja:
--a. rekurzivno
sumi 0 = 0
sumi x 
    | cifra /= 0 = cifra + sumi noviX
    | cifra == 0 && x > 0 = sumi noviX
    | otherwise = 0
    where cifra = mod x 10
          noviX = div x 10

--b. repno rekurzivno
sumirr 0 = 0
sumirr x = sumiAcc 0 x
sumiAcc acc 0 = acc
sumiAcc acc x 
    | cifra /= 0 = sumiAcc (cifra + acc) noviX
    | cifra == 0 && x > 0 = sumiAcc acc noviX
    | otherwise = 0
    where cifra = mod x 10
          noviX = div x 10

--Sumirati parne cifre nekog broja 
sumiParne 0 = 0
sumiParne x = sumiParneAcc 0 0 x
sumiParneAcc br acc 0 = acc
sumiParneAcc br acc x 
    | mod br 2 == 1 = sumiParneAcc (br + 1) acc noviX
    | cifra /= 0 = sumiParneAcc (br + 1) (cifra + acc) noviX
    | cifra == 0 && x > 0 = sumiParneAcc (br + 1) acc noviX
    | otherwise = 0
    where cifra = mod x 10
          noviX = div x 10