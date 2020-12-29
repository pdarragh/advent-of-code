import Data.List (foldl', group, sort)
import qualified Data.IntMap.Strict as IntMap

-- Deconstructs a list's head from the rest. Useful when you don't want to
-- pattern match, as in a function call.
uncons :: [a] -> (a, [a])
uncons [] = error "non-empty list"
uncons (x:xs) = (x, xs)

-- Returns the longest prefix (possibly empty) of a list `xs` of elements that
-- satisfy the given predicate.
takeUnconsWhile :: (a -> Bool) -> [a] -> [(a, [a])]
takeUnconsWhile _ [] = []
takeUnconsWhile p (x:xs)
  | p x       = (x, xs) : takeUnconsWhile p xs
  | otherwise = []

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

-- Counts the number of ways a sub-set of the given sequence could be formed
-- where the difference between successive elements is no more than the given
-- amount.
countWays :: Int -> [Int] -> Int
countWays d js = lookupOrUpdateMapFromHead (uncons ps) initialMemos IntMap.! 0
  where
    -- Construct the list of (index, value) pairs.
    ps :: [(Int, Int)]
    ps = zip [0..] js
    -- Construct an initial memoization table. There is only one way to choose
    -- the last element of the list.
    initialMemos :: IntMap.IntMap Int
    initialMemos = IntMap.singleton (fst (last ps)) 1
    -- Using the head of this funny list, attempt to look up the index in the
    -- given memoization table. If present, do nothing. If missing, compute the
    -- value for the index.
    lookupOrUpdateMapFromHead :: ((Int, Int), [(Int, Int)]) -> IntMap.IntMap Int -> IntMap.IntMap Int
    lookupOrUpdateMapFromHead ((idx, j), ps') memos = case IntMap.lookup idx memos of
      Just _  -> memos
      Nothing -> newMemos
        where
          -- Insert the current index's value into the memoization table.
          newMemos :: IntMap.IntMap Int
          newMemos = IntMap.insert idx v memosAfterNextSteps
          -- Compute the current index's value as the sum of the values of the
          -- next steps that could be taken.
          v :: Int
          v = sum (map ((memosAfterNextSteps IntMap.!) . fst . fst) nextSteps)
          -- Update the memoization table by recursing over the next steps.
          -- This is computed as an eager fold so that the memoization table is
          -- updated for each successive call, ensuring no false negative
          -- lookups.
          memosAfterNextSteps :: IntMap.IntMap Int
          memosAfterNextSteps = foldl' (flip lookupOrUpdateMapFromHead) memos nextSteps
          -- Identify which next steps could be made.
          nextSteps :: [((Int, Int), [(Int, Int)])]
          nextSteps = takeUnconsWhile ((<= j + d) . snd) ps'

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
  let ways = countWays 3 (sortAndAddSourceAndDevice joltages)
  putStrLn ("The number of ways to create a valid ordering of adapters is: " ++ show ways)
