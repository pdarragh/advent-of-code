{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor (($>))
import Data.List (sort)
import Text.ParserCombinators.ReadP (ReadP, char, choice, count)
import Text.Read (readPrec, readP_to_Prec)

sourceFile :: String
sourceFile = "input.txt"

class (Show a, Eq a) => Partition a where
  -- An instance of Partition defines a pair of elements that give the halves of
  -- a partition.
  halves :: (a, a)

  -- Partitions are divided into their `left` and `right` regions based on the
  -- given halves.
  left :: a
  left = fst halves

  right :: a
  right = snd halves

  -- A `left` or `right` can be read automatically. They are defined to be read
  -- by the first character of the string rendered by `show x`.
  readPartition :: ReadP a
  readPartition = choice [readPart left, readPart right] where
    readPart :: a -> ReadP a
    readPart x = char (head (show x)) $> x

  -- It is useful to be able to read a specific amount of partition elements.
  readPartitionListN :: Int -> ReadP [a]
  readPartitionListN n = count n readPartition

-- Rows have a "front half" and "back half".
data RowPartition = FrontHalf | BackHalf deriving (Eq, Show)
instance Partition RowPartition where
  halves = (FrontHalf, BackHalf)

-- Columns have a "left half" and "right half".
data ColPartition = LeftHalf | RightHalf deriving (Eq, Show)
instance Partition ColPartition where
  halves = (LeftHalf, RightHalf)

-- A boarding pass consists of a sequence of row partition specifiers and a
-- sequence of column partition specifiers.
data BoardingPass = BoardingPass { getRows :: [RowPartition]
                                 , getCols :: [ColPartition] }
                    deriving (Eq, Show)

readBoardingPass :: Int -> Int -> ReadP BoardingPass
readBoardingPass rowCount colCount = do
  rows <- readPartitionListN rowCount :: ReadP [RowPartition]
  cols <- readPartitionListN colCount :: ReadP [ColPartition]
  return (BoardingPass { getRows=rows, getCols=cols })

instance Read BoardingPass where
  -- In our case, boarding passes are 7 row specifiers and 3 column specifiers.
  readPrec = readP_to_Prec (const (readBoardingPass 7 3))

-- Computes the integer value corresponding to the full binary partition of the
-- given partition regions. Assumes the full region is the described by the
-- interval [0, 2 ^ (number of partitions to make) - 1].
partition :: Partition a => [a] -> Int
partition parts = fst (partition' 0 ((2 ^ length parts) - 1) parts)

-- Computes the range resulting from partitioning a specified bound with a
-- given number of binary partitions.
partition' :: Partition a => Int -> Int -> [a] -> (Int, Int)
partition' lo hi [] = (lo, hi)
partition' lo hi (p:ps)
  | p == left  = partition' lo (lo + newSize) ps
  | p == right = partition' (1 + lo + newSize) hi ps
  | otherwise  = error ("not a partition half marker: " ++ show p)
  where newSize = div (hi - lo) 2

-- Computes a seat ID.
seatID :: BoardingPass -> Int
seatID pass = (rowNum * 8) + colNum
  where rowNum = partition (getRows pass)
        colNum = partition (getCols pass)

-- Finds the largest seat ID among a group of boarding passes.
maxSeatID :: [BoardingPass] -> Int
maxSeatID passes = foldr max (head ids) (tail ids) where ids = map seatID passes

-- Given a set of mostly-consecutive numbers, find the one missing.
findMissingSeatID :: [Int] -> Maybe Int
findMissingSeatID seats = case missing of
                            [n] -> Just n
                            _   -> Nothing
  where
    missing = map ((+ 1) . fst) (filter (not . seatsAreAdjacent) adjacentSeatIDS)
      where seatsAreAdjacent :: (Int, Int) -> Bool
            seatsAreAdjacent (l, r) = r == (l + 1)
            sortedSeats = sort seats
            adjacentSeatIDS = zip (init sortedSeats) (tail sortedSeats)

-- Converts a file into a list of boarding passes.
readInputFile :: String -> IO [BoardingPass]
readInputFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

main :: IO ()
main = do
  passes <- readInputFile sourceFile
  let largestSeatID = maxSeatID passes
  putStrLn ("The largest seat ID is: " ++ show largestSeatID ++ ".")
  case findMissingSeatID (map seatID passes) of
    Nothing -> putStrLn "Could not find the missing seat ID."
    Just seat -> putStrLn ("The missing seat ID is: " ++ show seat ++ ".")
