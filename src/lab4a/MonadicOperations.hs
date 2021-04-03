module MonadicOperations where

import           Prelude hiding (sequence)

-- W zadaniu chodzi o to żeby napisać funkcje monadyczne,
-- które normalnie znajdują się w Control.Monad

sequence :: (Monad m) => [m a] -> m [a]
sequence [] = pure []
sequence (x:xs) = do
  ans <- x
  rest <- sequence xs
  pure (ans:rest)

-- żeby zorientować się o co chodzi, można zastąpić
-- m jakąś znaną sobie monadą:
--sequence :: [IO a] -> IO [a]
--sequence :: [Maybe a] -> Maybe [a]


forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM l f = sequence $ map f l


mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM = flip forM


-- ignoruj obliczaną wartość
void :: (Monad m) => m a -> m ()
void x = do
    x
    pure ()


sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = void . sequence


forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ l f = void $ forM l f


-- Wykonaj akcję tylko jeśli pierwszy argument to true
when :: (Monad m) => Bool -> m () -> m ()
when True action = action
when False _     = pure()


-- (Przeciwieństwo when to unless)

-- Wykonuj akcję w nieskończoność
forever :: (Monad m) => m () -> m ()
forever action = do
    action
    forever action


-- forever ma sens dla IO, ale dla Maybe raczej nie
-- jest pomocne

-- tak naprawdę:
--  forever :: (Monad m) => m a -> m b
-- dlaczego?

-- monadyczyny odpowiednik foldl:
foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM f init [] = pure init
foldM f current (x:xs) = do
    current' <- f current x
    foldM f current' xs
