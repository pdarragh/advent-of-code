{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, char, choice, count)
import Text.Read (readPrec, readP_to_Prec)

sourceFile :: String
sourceFile = "input.txt"

class Show a => Partition a where
  -- An instance of Partition defines an indicator for "lower half" and "upper
  -- half", which we call `lo` and `hi`, respectively.
  lo :: a
  hi :: a

  -- A `lo` or `hi` can be read automatically. They are defined to be read by
  -- the first character of the string rendered by `show x`.
  readPartition :: ReadP a
  readPartition = choice [readPart lo, readPart hi] where
    readPart :: a -> ReadP a
    readPart x = char (head (show x)) $> x

  -- It is useful to be able to read a specific amount of partition elements.
  readPartitionListN :: Int -> ReadP [a]
  readPartitionListN n = count n readPartition

-- Rows have a "front half" and "back half".
data RowPartition = FrontHalf | BackHalf deriving (Eq, Show)
instance Partition RowPartition where
  lo = FrontHalf
  hi = BackHalf

-- Columns have a "left half" and "right half".
data ColPartition = LeftHalf | RightHalf deriving (Eq, Show)
instance Partition ColPartition where
  lo = LeftHalf
  hi = RightHalf

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

-- Converts a file into a list of boarding passes.
readInputFile :: String -> IO [BoardingPass]
readInputFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

main :: IO ()
main = do
  passes <- readInputFile sourceFile
  mapM_ print passes
