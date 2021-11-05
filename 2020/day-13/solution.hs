import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy, skipMany, skipMany1)
import Text.Read (readPrec, readP_to_Prec)
import Control.Applicative ((<|>))

readBusField :: ReadP (Maybe Int)
readBusField = Just . read <$> many1 (satisfy isDigit) <|>
               Nothing <$ char 'x'

newtype BusInput = BusInput (Int, [Maybe Int])

readInput :: ReadP BusInput
readInput = do
  time <- many1 (satisfy isDigit) <* skipMany1 (char '\n')
  busFields <- sepBy readBusField (char ',')
  return (BusInput (read time, busFields))

instance Read BusInput where
  readPrec = readP_to_Prec (const readInput)

findSoonestID :: Int -> [Int] -> (Int, Int)
findSoonestID time bids =
  foldl1
    ( \t1@(mbid, mdiff) t2@(bid, diff) ->
        if mdiff < diff then t1 else t2
    )
    (map processID bids)
  where
    processID :: Int -> (Int, Int)
    processID bid = (bid, bid - (time `rem` bid))

readInputFile :: String -> IO (Int, [Maybe Int])
readInputFile fileName = do
  content <- readFile fileName
  let BusInput (time, ids) = read content
  return (time, ids)

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  (time, ids) <- readInputFile sourceFile
  let (id, diff) = findSoonestID time (catMaybes ids)
  putStrLn ("Soonest ID: " ++ show id)
  putStrLn ("Diff: " ++ show diff)
  putStrLn ("  ID * Diff = " ++ show (id * diff))
