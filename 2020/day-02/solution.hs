import Data.Char (isSpace)
import Data.List (elemIndex)

sourceFile :: String
sourceFile = "input.txt"

data Requirement = Requirement
  { minCount :: Int
  , maxCount :: Int
  , char     :: Char
  } deriving (Show)

type Password = String

processRequirement :: String -> Requirement
processRequirement s =
  Requirement { minCount = lo
              , maxCount = hi
              , char = head rawChar
              } where
  [range, rawChar] = words s
  [lo, hi] = map read (splitString '-' 1 range)

splitString :: Char -> Int -> String -> [String]
splitString _   0 s = [s]
splitString sep c s = case elemIndex sep s of
  Nothing    -> [s]
  Just index -> (take index s) : (splitString sep (c - 1) (drop (index + 1) s))

readInputFile :: String -> IO [(Requirement, Password)]
readInputFile fileName = do
  content <- readFile fileName
  return (map readLine (lines content))
  where
    readLine :: String -> (Requirement, Password)
    readLine line = (requirement, password) where
      [rawRequirement, rawPassword] = splitString ':' 1 line
      requirement = processRequirement rawRequirement
      password = dropWhile isSpace rawPassword

main :: IO ()
main = do
  reqsAndPasses <- readInputFile sourceFile
  mapM_ print reqsAndPasses
