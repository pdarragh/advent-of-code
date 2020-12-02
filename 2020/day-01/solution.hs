import qualified Data.Map.Strict as Map

sourceFile :: String
sourceFile = "input.txt"

targetSum :: Int
targetSum = 2020

-- Finds two integers in a list of integers that sum to a given integer.
--
-- When a new number is processed (i.e., in each call to twoSum'), its desired
-- pair is calculated as the difference from the target number. The map is then
-- updated so the result of that calculation (target - current) maps to the
-- index of the current number. So in the future, we can determine whether we
-- have found the pair by looking up the current number in the map and, if a
-- match is found, using the index stored in the map to get back the previous
-- number.
twoSum :: Int -> [Int] -> Maybe (Int, Int)
twoSum target numbers = twoSum' 0 Map.empty where
  twoSum' :: Int -> Map.Map Int Int -> Maybe (Int, Int)
  twoSum' index pairMap
    | index >= length numbers = Nothing
    | otherwise =
      let current = numbers !! index in
        case Map.lookup current pairMap of
          Nothing        -> twoSum' (index + 1) (Map.insert (target - current) index pairMap)
          Just pairIndex -> Just (current, numbers !! pairIndex)

-- Finds three integers in a list of integers that sum to a given integer.
--
-- Let us assume we have:
--
--     t = a + b + c
--
-- where `t` is the target integer and `a`, `b`, and `c` are numbers in the
-- list.
--
-- This function works by iterating over the list of numbers and assuming each
-- number fills the role of `a` in the above equation. It then calls `twoSum`
-- with the quantity `t - a`, essentially asking `twoSum` to find two values `b`
-- and `c` in the list of numbers that sum to `t - a`. If successful, `a`, `b`,
-- and `c` form a triple of numbers that sum to `t`.
threeSum :: Int -> [Int] -> Maybe (Int, Int, Int)
threeSum target numbers = threeSum' incompletePairs where
  -- The list of adjusted `twoSum` target values.
  incompletePairs :: [(Int, Int)]  -- [(pairIndex, pairTarget)]
  incompletePairs = zip [0..] (map (target -) numbers)
  -- The real implementation.
  threeSum' :: [(Int, Int)] -> Maybe (Int, Int, Int)
  threeSum' [] = Nothing
  threeSum' ((pairIndex, pairTarget):pairs) =
    case twoSum pairTarget numbers of
      Nothing     -> threeSum' pairs
      Just (a, b) -> Just (a, b, numbers !! pairIndex)

-- Converts a file whose contents are lines with integers on them into a list
-- of integers.
readIntsFromFile :: String -> IO [Int]
readIntsFromFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

main :: IO ()
main = do
  numbers <- readIntsFromFile sourceFile
  putStrLn (case twoSum targetSum numbers of
    Nothing     -> "List did not contain two numbers that sum to " ++ show targetSum ++ "."
    Just (a, b) -> show targetSum ++ " = " ++ show a ++ " + " ++ show b ++ ". a * b = " ++ show (a * b))
  putStrLn (case threeSum targetSum numbers of
    Nothing        -> "List did not contain three numbers that sum to " ++ show targetSum ++ "."
    Just (a, b, c) -> show targetSum ++ " = " ++ show a ++ " + " ++ show b ++ " + " ++ show c ++ ". a * b * c = " ++ show (a * b * c))
