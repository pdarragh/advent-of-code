import Data.List (group, sort)

-- Produces a list of the difference between each consecutive pair of elements
-- in the original list. For an input list with size x, the returned list will
-- be of size (x - 1).
differences :: [Int] -> [Int]
differences xs = zipWith (flip (-)) xs (tail xs)

-- Computes the differences between the sorted elements of the given list and
-- groups those differences.
groupedDifferences :: [Int] -> [[Int]]
groupedDifferences = group . sort . differences

-- Produces a list of all the instances of pairwise-consecutive differences of
-- just 1 or 3.
oneAndThreeDifferences :: [[Int]] -> [[Int]]
oneAndThreeDifferences = filter (isOneOrThree . head)
  where
    isOneOrThree :: Int -> Bool
    isOneOrThree x
      | x == 1 || x == 3 = True
      | otherwise        = False

-- Counts the number of differences of 1 or 3 in the sorted input list.
quantitiesOfOndAndThreeDifferences :: [Int] -> [Int]
quantitiesOfOndAndThreeDifferences = map length . oneAndThreeDifferences . groupedDifferences

-- Multiplies the differences of 1 or 3 in the sorted input list.
productOfOneAndThreeDifferences :: [Int] -> Int
productOfOneAndThreeDifferences = product . quantitiesOfOndAndThreeDifferences

-- Sorts the given list, adding 0 to the front and a new element at the end
-- equal to 3 plus the previous greatest element.
sortAndAddSourceAndDevice :: [Int] -> [Int]
sortAndAddSourceAndDevice xs = 0 : xs' ++ [3 + last xs']
  where
    xs' = sort xs

readInputFile :: String -> IO [Int]
readInputFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  joltages <- readInputFile sourceFile
  let oneOrThreeProduct = productOfOneAndThreeDifferences (sortAndAddSourceAndDevice joltages)
  putStrLn ("The product of the 1-jolt differences and the 3-jolt differences is: " ++ show oneOrThreeProduct)
