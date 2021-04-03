{-# LANGUAGE DeriveFunctor #-}
module StateDemo where

import           Control.Monad.State


-- Zdefinujmy nowy typ drzewa który trzyma wartość w każdym wierzchołku

data Tree a = Nill | Node (Tree a) a (Tree a)
  deriving (Show, Read, Eq, Ord, Functor)

-- Napiszmy funkcję "normalnie"

enumeratePostorder :: Tree a -> Tree (Int, a)
enumeratePostorder tree = fst $ enumeratePostorder tree 0


-- enumeratePostorederAux dostaje numer na wejściu i zwraca ponumerowane
-- drzewo i nowy numer (pierwszy nieużywany)
enumeratePostorderAux :: Tree a -> Int -> (Tree (Int, a), Int)
enumeratePostorderAux Nill n = (Nill, n)
enumeratePostorderAux (Node left val right) n = (Node left' n'' right', n'' + 1)
  where
    (left', n') = enumeratePostorderAux left n
    (right', n'') = enumeratePostorderAux right n'


-- Możemy ten problem rozwiązać też za pomocą monady stanu

enumeratePostorderMonadicAux :: Tree a -> State Int (Tree (Int, a))
enumeratePostorderMonadicAux Nill = pure Nill
enumeratePostorderMonadicAux (Node left val right) = do
  left' <- enumeratePostorderMonadicAux left
  right' <- enumeratePostorderMonadicAux right
  n <- get
  put $ n + 1
  pure $ Node left' (n, val) right'

enumeratePostorderMonadic :: Tree a -> Tree (Int, a)
enumeratePostorderMonadic t = evalState (enumeratePostorderMonadicAux t) 0


-----------------------------------------------------------

-- Czym jest monada State a s?
-- To są właśnie funkcje a -> (a, s)

type MyState s a = s -> (a, s)

newtype MyState' s a = MyState' {runMyState :: s -> (a, s)}
-- zwykły rekord z jednym polem

myPure :: a ->  MyState s a
myPure x =  MyState $ \s -> (x,s)

myBind ::  MyState' s a -> (a ->  MyState' s b) -> MyState s b
myBind ( MyState' f) g =  MyState' $ \s ->
  let (a, s') = f s in
  let ( MyState' ga) = g a in
  ga s'

--------------
-- Przykład --
--------------

myTree :: Tree ()
myTree = Node
  (Node
    (Node Nill () Nill)
    ()
    (Node Nill () Nill))
  ()
  (Node Nill () Nill)


------------------
-- Do zrobienia --
------------------

-- enumeratePreorder, enumerateInorder

-- zadanie z codewars

-- napisać instancję Monad dla MyState'
