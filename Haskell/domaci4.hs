-- 1. Kreirati novi data tip Stablo, koji moze imati elemente Nista
--    i Cvor, gde Cvor sadrzi nosi neki Int, kao i levo i desno podstablo
--    (koji su tipa Stablo).
data Stablo = Nista | Cvor Int Stablo Stablo deriving Show

-- 2. Napisati funkciju "sadrzi" koja prima Int i Stablo, te pretrazuje
--    da li se prosledjeni Int nalazi u stablu. Vraca Boolean vrednost.
sadrzi i Nista = False
sadrzi i (Cvor a levo desno) 
    | i == a = True
    | otherwise = (sadrzi i levo) || (sadrzi i desno)

-- 3. Napisati funkciju uListu koja sve brojeve u stablu smesta u listu.
uListu Nista = []
uListu (Cvor a levo desno) = uListu levo : a : uListu desno

-- 4. Napisati funkciju koja vraca listu svih brojeva u stablu
--    koji su deljivi sa 3 ili 5.
f4 Nista = []
f4 l = filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) (uListu l)

-- 5. Napisati funkciju preslikaj koja prima stablo i preslika ga
--    "u ogledalu". (Obrne levo i desno podstablo).
preslikaj Nista = Nista
preslikaj (Cvor a levo desno) = Cvor a (preslikaj desno) (preslikaj levo)

-- 6. Napisati funkciju filterStablo. Ona kao parametre ocekuje funkciju f
--    koja prima Int i vraca Bool, i Stablo. Rezultat funkcije filterStablo
--    je lista brojeva iz stabla koji vrate True primenom funkcije f.



-- -- 1)

-- data Stablo = 
--     Nista 
--     | Cvor Int Stablo Stablo 
--     deriving Show

-- -- 2)

-- sadrzi :: Int -&gt; Stablo -&gt; Bool
-- sadrzi _ Nista = False
-- sadrzi n (Cvor br levo desno)
--     | n == br   = True
--     | otherwise = (sadrzi n levo) || (sadrzi n desno)

-- -- 3)

-- uListu :: Stablo -&gt; [Int]
-- uListu Nista = []
-- uListu (Cvor br levo desno) = (uListu levo) ++ [br] ++ (uListu desno)

-- -- 4)

-- deljiviSa3i5 :: Stablo -&gt; [Int]
-- deljiviSa3i5 Nista = []
-- deljiviSa3i5 (Cvor br levo desno)
--     | odgovara  = (deljiviSa3i5 levo) ++ [br] ++ (deljiviSa3i5 desno)
--     | otherwise = (deljiviSa3i5 levo) ++ (deljiviSa3i5 desno)
--     where
--         odgovara = mod br 3 == 0 || mod br 5 == 0

-- -- 5)

-- preslikaj :: Stablo -&gt; Stablo
-- preslikaj Nista = Nista
-- preslikaj (Cvor br levo desno) = Cvor br (preslikaj desno) (preslikaj levo)

-- -- 6)

-- filterStablo :: (Int -&gt; Bool) -&gt; Stablo -&gt; [Int]
-- filterStablo _ Nista = []
-- filterStablo f (Cvor br levo desno)
--     | odgovara  = (filterStablo f levo) ++ [br] ++ (filterStablo f desno)
--     | otherwise = (filterStablo f levo) ++ (filterStablo f desno)
--     where
--         odgovara = f br