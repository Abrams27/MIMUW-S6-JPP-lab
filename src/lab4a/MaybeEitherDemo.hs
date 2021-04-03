module MaybeEitherDemo where

import           Control.Monad (guard)
import           Text.Read     (readMaybe)

-- Chcemy napisać funkcję
-- parseAndDivide :: String -> String
-- która przyjmie napis postaci
-- "a / b"
-- gdzie a i b to liczby całkowite
-- i wypisze wynik ich dzielenia lub "ERROR"
-- np.
-- parseAndDivide "10 / 5" = "2"
-- parseAndDivide "10 / 5 17" = "ERROR"
-- parseAndDivide "10 / 0" = "ERROR"

-- zacznijmy od
maybeDiv :: Int -> Int -> Maybe Int
maybeDiv _ 0 = Nothing
maybeDiv a b = Just $ a `div` b

-- teraz napiszmy
parseInput :: String -> Maybe (Int, Int)
parseInput input = do
  let args = words input
  guard $ length args == 3
  let [x, operator, y] = args
  guard $ operator == "/"
  mx <- readMaybe x
  my <- readMaybe y
  pure (mx, my)

--teraz napiszmy
makeOutput :: Maybe Int -> String
makeOutput Nothing  = "ERROR"
makeOutput (Just x) = show x

--i w końcu
parseAndDivide :: String -> String
parseAndDivide input = makeOutput $ do
  (x, y) <- parseInput input
  maybeDiv x y


-- Powiedzmy, że chcielibyśmy mieć dokładne
-- infomracje o błędzie.
-- divide  "10 / 0" = "ERROR: Division by zero"
-- divide  "10 / 5 7" = "ERROR: Too many arguments"
-- divide  "10a / 10" = "ERROR: First argument is not a number"parseAndDivide
-- w przypadku wielu błędów raportujemy tylko jeden (dowolny)

-- w tym celu napiszemy funkcję
-- divideEither :: String -> String

type Error = String

type EitherErrorOr a = Either Error a


makeError :: String -> EitherErrorOr a
makeError = Left


eitherDiv :: Int -> Int -> EitherErrorOr Int
eitherDiv _ 0 = makeError "Division by zero"
eitherDiv a b = pure $ a `div` b


guardOrError :: Error -> Bool -> EitherErrorOr ()
guardOrError _ True    = pure ()
guardOrError err False = makeError err


fromMaybeOrError :: Error -> Maybe a -> EitherErrorOr a
fromMaybeOrError _ (Just x)  = pure x
fromMaybeOrError err Nothing = makeError err


parseInputEither :: String -> EitherErrorOr (Int, Int)
parseInputEither input = do
  let args = words input
  guardOrError "Not enough arguments" $ length args >= 3
  guardOrError "Too many arguments" $ length args <= 3

  let [x, operator, y] = args
  guardOrError "The operator is not /" $ op == "/"
  xm <- fromMaybeOrError "First argument is not a number" $ readMaybe x
  ym <- fromMaybeOrError "Second argument is not a number" $ readMaybe y
  pure (xm, ym)


makeOutputEither :: EitherErrorOr Int -> String
makeOutputEither (Left err)  = "ERROR: " ++ err
makeOutputEither (Right ans) = show ans


parseAndDivideEither :: String -> String
parseAndDivideEither input = makeOutputEither $ do
  (x, y) <- parseInputEither input
  eitherDiv x y
