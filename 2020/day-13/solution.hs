import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromJust, mapMaybe, isJust)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy, skipMany, skipMany1)
import Text.Read (readPrec, readP_to_Prec)
import Control.Applicative ((<|>))

import Control.Concurrent (threadDelay)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (maximumBy)
import Data.Ord (comparing)

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

-- iterate multiples of largest bus ID's schedule
-- check if divisible by first ID
--   if yes, check if value + 1 divisible by second ID

findOffsets :: [Maybe Int] -> [(Int, Int)]
findOffsets = map (second fromJust) . filter (isJust . snd) . zip [0..]

findCommonTimestamp :: [(Int, Int)] -> Int
findCommonTimestamp offsets =
  head $ mapMaybe processTimestamp timestamps
  where
    timestamps = [-offset, -offset + multiplier ..]
    (offset, multiplier) = maximumBy (comparing snd) offsets
    processTimestamp :: Int -> Maybe Int
    processTimestamp = processTimestamp' offsets
    processTimestamp' :: [(Int, Int)] -> Int -> Maybe Int
    processTimestamp' [] t = Just t
    processTimestamp' ((offset, diff):offsets') t =
      if (t + offset) `rem` diff == 0
      then processTimestamp' offsets' t
      else Nothing

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
  putStrLn "Part 1"
  putStrLn ("  Soonest ID: " ++ show id)
  putStrLn ("  Diff: " ++ show diff)
  putStrLn ("    ID * Diff = " ++ show (id * diff))
  let offsets = findOffsets ids
  let timestamp = findCommonTimestamp offsets
  putStrLn "Part 2"
  putStrLn ("  Timestamp: " ++ show timestamp)
