module Folds where 

import Prelude hiding (foldl, foldr)

-- rozwiązanie można sprawdzić odpalając `runhaskell FoldsTest.hs`


-- Poniższa funkcja ma działać następująco
-- foldl f z [x1, x2, ..., zn] == (...((z `f` x_1) `f` x_2) ... `f` xn)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = undefined


-- Poniższa funkcja ma działać następująco:
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z) ... ) 
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = undefined


-- Wszystkie poniższe funkcje powinny wywoływać folda i od razu zwracać jego wynik
-- (nie powinny go przetwarzać).

-- Poniższa funkcja używa foldl, żeby policzyć długość listy
lengthFoldl :: [a] -> Int
lengthFoldl = foldl undefined undefined


-- A poniższa używa foldr:
lengthFoldr :: [a] -> Int
lengthFoldr = foldr undefined undefined


-- Znajdź takie
strangeF :: Int -> Int -> Int
strangeF = undefined
-- żeby działało inaczej z foldemr i foldeml:
strangeFTest :: Bool
strangeFTest = foldl strangeF 1 [1, 2, 3] /= foldr (flip strangeF) 1 [1, 2, 3]
-- Można użyć hoogla, żeby dowiedzieć się co robi `flip`


-- Poniższa funkcja używa foldl, żeby zaimplementować reverse
reverseFoldl :: [a] -> [a]
reverseFoldl xs = foldl f undefined undefined
  where
    f :: [a] -> a -> [a]
    f = undefined


-- A poniższa foldr:
reverseFoldr :: [a] -> [a]
reverseFoldr = undefined
-- Która działa szybciej?


-- Poniższa fukcja używa któregoś z foldów, żeby zaimplementować nub (usuwanie powtórzeń)
nubFold :: (Eq a) => [a] -> [a]
nubFold = undefined
-- Podpowiedź: Przydatna może być funkcja `elem`
-- LMHTFY: https://hoogle.haskell.org/?hoogle=elem&scope=set%3Astackage


-- Poniższa funkcja używa któregoś z foldów, żeby zaimplementować elem :)
elemFold :: (Eq a) => a -> [a] -> Bool
elemFold e = undefined


-- Poniższa funkcja używa któregoś z foldów, żeby sprawdzić czy dane słowo zawiera "ab" jako podsłowo
containsabFold :: String -> Bool
containsabFold = undefined
