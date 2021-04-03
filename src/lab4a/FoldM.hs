module FoldM where

import           Control.Monad


-- Chcemy zaimplementować wielokrotne dzielenie:
--   [100, 2, 3, 4] --> (((100 / 2) / 3) / 4)

repDiv :: [Int] -> Int
repDiv (x:xs) = foldl div x xs

-- repDiv [100, 0, 3, 4] kończy się błędem
-- repDiv []             też


maybeDiv :: Int -> Int -> Maybe Int
maybeDiv _ 0 = Nothing
maybeDiv a b = Just $ a `div` b


-- foldM :: (b -> a -> m b) -> b -> [a] -> m b
repDivM :: [Int] -> Maybe Int
repDivM []     = Nothing
repDivM (x:xs) = foldM maybeDiv x xs
