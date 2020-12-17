import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, eof, get, manyTill, munch1)
import Text.Read (readPrec, readP_to_Prec)

-- A semantic type alias.
type Password = String

-- The specification of what is required of a given password.
data Requirement = Requirement
  { numOne  :: Int
  , numTwo  :: Int
  , character :: Char
  }

instance Show Requirement where
  -- Renders a password requirement according to the rules of the input file.
  show req = show (numOne req) ++ "-" ++ show (numTwo req) ++ " " ++ show (character req)

-- Defines a parser for reading a password requirement.
readRequirement :: ReadP Requirement
readRequirement = do n1 <- munch1 isDigit
                     _  <- char '-'
                     n2 <- munch1 isDigit
                     _  <- char ' '
                     c  <- get
                     return Requirement { numOne = read n1
                                        , numTwo = read n2
                                        , character = c }

instance Read Requirement where
  -- Reads a password requirement from a string.
  readPrec = readP_to_Prec (const readRequirement)

-- A simple pair. This is used so we can have a custom `read` definition. :)
data ReqPassPair = ReqPassPair Requirement Password

-- Enable easy unpairing of this dumb datatype.
unpair :: ReqPassPair -> (Requirement, Password)
unpair (ReqPassPair requirement password) = (requirement, password)

-- Defines a parser for reading a password requirement and a password,
-- according to the rules of the input file.
readReqPassPair :: ReadP ReqPassPair
readReqPassPair = do req  <- readRequirement
                     _    <- char ':'
                     _    <- char ' '
                     pass <- manyTill get eof
                     return (ReqPassPair req pass)

instance Read ReqPassPair where
  -- Reads a password requirement and password from a string.
  readPrec = readP_to_Prec (const readReqPassPair)

-- Determines whether a password is valid according to a `Requirement`, per the
-- rules of Part 1. Here, `numOne` and `numTwo` of the requirement represent the
-- bounds of a range of the quantity of allowable instances of the specified
-- `character`.
passwordValid1 :: Requirement -> Password -> Bool
passwordValid1 req = passwordValid' 0 where
  passwordValid' :: Int -> Password -> Bool
  passwordValid' count "" = (count >= numOne req) && (count <= numTwo req)
  passwordValid' count (c:pass')
    | c == character req = numTwo req /= 0 && passwordValid' (count + 1) pass'
    | otherwise          = passwordValid' count pass'

-- Determines whether a password is valid according to a `Requirement`, per the
-- rules of Part 2. Here, `numOne` and `numTwo` of the requirement represent
-- specific *1-indexed* positions in the password where the specified
-- `character` must appear. The character must appear in one or the other, but
-- not both or neither.
passwordValid2 :: Requirement -> Password -> Bool
passwordValid2 Requirement { numOne = d1, numTwo = d2, character = c } password
  | d1 > length password || d2 > length password = False
  | otherwise = case (password !! (d1 - 1) == c, password !! (d2 - 1) == c) of
      (True,  True)  -> False
      (False, False) -> False
      (_,     _)     -> True

-- Converts a file whose contents are lines of a specific format into a list of
-- pairs of password requirements and passwords. The format is:
--
--     n1-n2 c: password
--
-- where `n1` and `n2` are positive integers such that `n2` is greater than
-- `n1`, `c` is a single character, and `password` is the rest of the string
-- after the space following the colon.
readInputFile :: String -> IO [(Requirement, Password)]
readInputFile fileName = do
  content <- readFile fileName
  return (map (unpair . read) (lines content))

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  reqsAndPasses <- readInputFile sourceFile
  let count1 = length (filter (uncurry passwordValid1) reqsAndPasses)
      count2 = length (filter (uncurry passwordValid2) reqsAndPasses)
  putStrLn ("Number of valid-1 passwords in input: " ++ show count1)
  putStrLn ("Number of valid-2 passwords in input: " ++ show count2)
