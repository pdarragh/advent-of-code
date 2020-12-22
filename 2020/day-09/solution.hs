import Data.List (tails)
import Data.Sequence ((|>), Seq(..))

-- Safely produces the head of a list, if it exists.
maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

-- Unsafely produces the right tail of a sequence (the sequence without the left
-- head), raising an error if the sequence is empty.
rightTail :: Seq a -> Seq a
rightTail Empty = error "empty sequence"
rightTail (_:<|t) = t

-- Given a target integer and a pair of integers, determines whether the target
-- is equal to the sum of the pair.
(==.+) :: Int -> (Int, Int) -> Bool
(==.+) = flip ((==) . uncurry (+))

-- Extracts all sublists of a given length from a list.
sublistsOfLength :: Int -> [a] -> [[a]]
sublistsOfLength len xs
  | length xs < len = []
  | otherwise = take len xs : sublistsOfLength len (drop 1 xs)

-- Produces windows over a list by pairing each element with the preceding
-- sublist of a given length. Returns an empty list if the specified length is
-- too large.
--
-- NOTE: To avoid getting an empty list, the list must be at least `len` + 1 in
--       size, because the element cannot be a member of its own window.
windows :: Int -> [a] -> [(a, [a])]
windows len xs = zip (drop len xs) (sublistsOfLength len xs)

-- Produces a list of unique pairs from a list, not including reversed pairs.
pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

-- Determines whether a target element is expressible as a pair within a list,
-- according to the given function.
expressibleAsPair :: (a -> (a, a) -> Bool) -> a -> [a] -> Bool
expressibleAsPair f t xs = any (f t) (pairs xs)

-- Produces a list of elements which are not expressible as pairs within their
-- corresponding windows, according to the given function.
elemsNotExpressibleAsPairsInWindows :: (a -> (a, a) -> Bool) -> Int -> [a] -> [a]
elemsNotExpressibleAsPairsInWindows f len xs = map fst (filter (not . uncurry (expressibleAsPair f)) (windows len xs))

-- Produces a list of integers which are not expressible as sums of pairs within
-- their corresponding windows.
numbersNotExpressibleAsPairsInWindows :: Int -> [Int] -> [Int]
numbersNotExpressibleAsPairsInWindows = elemsNotExpressibleAsPairsInWindows (==.+)

-- Attempts to find the contiguous subsequence of a list of numbers that sum to
-- a given target value.
sublistSum :: Int -> [Int] -> Seq Int
sublistSum target = sublistSum' Empty
  where
    sublistSum' :: Seq Int -> [Int] -> Seq Int
    sublistSum' buffer ns = case compare (sum buffer) target of
      EQ -> buffer
      LT -> sublistSum' (buffer |> head ns) (tail ns)
      GT -> sublistSum' (rightTail buffer) ns

-- Converts the input file to a list of integers.
readInputFile :: String -> IO [Int]
readInputFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

sourceFile :: String
sourceFile = "input.txt"

windowSize :: Int
windowSize = 25

main :: IO ()
main = do
  nums <- readInputFile sourceFile
  case maybeHead (numbersNotExpressibleAsPairsInWindows windowSize nums) of
    Nothing -> putStrLn "All numbers are expressible as pairs in their preceding windows."
    Just x  -> do
      putStrLn ("The first number not expressible as a pair in its preceding window is: " ++ show x)
      let sumSequence = sublistSum x nums
          lo = minimum sumSequence
          hi = maximum sumSequence
          pairSum = lo + hi
      putStrLn ("The sum of the first and last elements of the subsequence that sums to that number is: " ++ show pairSum)
