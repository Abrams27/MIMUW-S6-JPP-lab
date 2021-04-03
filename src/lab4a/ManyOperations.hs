module ManyOperations where

import Text.Read
import Control.Monad

-- Teraz chcemy mieć funkcje
-- performOperations :: String -> String
-- która wykona operacje arytmetyczne od lewej do prawej
-- czyli operacje "10 / 2 + 3 * 1 - 2" rozumiemy jako
-- ((((10 / 2) + 3) * 2) - 2) = 14
-- tak więc
-- performOperations "10 / 2 + 3 * 1 - 2" = "14"
-- a
-- performOperations "10 + +" = "ERROR"
-- performOperations "10 / 0" = "ERROR"
-- (na razie chcemy raportować niedokładny błąd)

-- Ładnie będzie najpierw sparsować wejście, a potem je obliczyć

data BinaryOp = Plus | Minus | Times | Div

type Operation = (BinaryOp, Int)

-- Piszemy funkcję parseInput
-- parseInput "10 / 2 + 3 - 2" =
--   Just (10, [(Div, 2), (Plus 3), (Minus 2)])

parseInput :: String -> Maybe (Int, [Operation])
parseInput input = do
    (firstValueString:restOperations) <- pure $ words input
    firstValue <- readMaybe firstValueString
    operations <- parseOperations restOperations
    pure (firstValue, operations)


parseOperations :: [String] -> Maybe [Operation]
parseOperations [] = pure []
parseOperations [x] = Nothing
parseOperations (operator:number:restOperations) = do
    binaryOp <- parseBinaryOp operator
    numberM <- readMaybe number
    followingOperations <- parseOperations restOperations
    pure $ (binaryOp, numberM) : followingOperations


parseBinaryOp :: String -> Maybe BinaryOp
parseBinaryOp "+" = pure Plus
parseBinaryOp "-" = pure Minus
parseBinaryOp "/" = pure Div
parseBinaryOp "*" = pure Times
parseBinaryOp _   = Nothing


executeOperations :: Int -> [Operation] -> Maybe Int
executeOperations = foldM performOperation
  where
    performOperation :: Int -> Operation -> Maybe Int
    performOperation x (Plus, y)  = pure $ x + y
    performOperation x (Times, y) = pure $ x * y
    performOperation x (Minus, y) = pure $ x - y
    performOperation x (Div, 0)   = Nothing
    performOperation x (Div, y)   = pure $ x `div` y


makeOutput :: Maybe Int -> String
makeOutput (Just ans) = show ans
makeOutput Nothing    = "ERROR"


performOperations :: String -> String
performOperations input = makeOutput $ do
    (firstValue, operators) <- parseInput input
    executeOperations firstValue operators

