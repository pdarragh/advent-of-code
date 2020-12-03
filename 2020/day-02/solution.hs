import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, eof, get, manyTill, munch1)
import Text.Read (readPrec, readP_to_Prec)

sourceFile :: String
sourceFile = "input.txt"

-- A semantic type alias.
type Password = String

-- The specification of what is required of a given password.
data Requirement = Requirement
  { minCount  :: Int
  , maxCount  :: Int
  , character :: Char
  }

instance Show Requirement where
  -- Renders a password requirement according to the rules of the input file.
  show req = show (minCount req) ++ "-" ++ show (maxCount req) ++ " " ++ show (character req)

-- Defines a parser for reading a password requirement.
readRequirement :: ReadP Requirement
readRequirement = do lo <- munch1 isDigit
                     _  <- char '-'
                     hi <- munch1 isDigit
                     _  <- char ' '
                     c  <- get
                     return Requirement { minCount = read lo
                                        , maxCount = read hi
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

-- Determines whether a password is valid according to a `Requirement`.
passwordValid :: Requirement -> Password -> Bool
passwordValid req = passwordValid' 0 where
  passwordValid' :: Int -> Password -> Bool
  passwordValid' count "" = (count >= minCount req) && (count <= maxCount req)
  passwordValid' count (c:pass')
    | c == character req = maxCount req /= 0 && passwordValid' (count + 1) pass'
    | otherwise     = passwordValid' (count + 1) pass'

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

main :: IO ()
main = do
  reqsAndPasses <- readInputFile sourceFile
  mapM_ (print . uncurry passwordValid) reqsAndPasses
