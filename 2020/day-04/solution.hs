import Control.Monad (liftM2, replicateM)
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
import Text.Read (readPrec, readP_to_Prec)

sourceFile :: String
sourceFile = "input.txt"

data Field
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving (Eq)

fieldLabels :: [(Field, String)]
fieldLabels =
  [ (BirthYear,      "byr")
  , (IssueYear,      "iyr")
  , (ExpirationYear, "eyr")
  , (Height,         "hgt")
  , (HairColor,      "hcl")
  , (EyeColor,       "ecl")
  , (PassportID,     "pid")
  , (CountryID,      "cid")
  ]

reverseLookup :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookup key pairs = lookup key (map swap pairs)

instance Show Field where
  show field = fromJust (lookup field fieldLabels)

getN :: Int -> ReadP String
getN n = replicateM n get

readField :: ReadP Field
readField = do
  label <- getN 3
  case reverseLookup label fieldLabels of
    Nothing    -> pfail
    Just field -> return field

instance Read Field where
  readPrec = readP_to_Prec (const readField)

data Specification = Specification Field String

instance Show Specification where
  show (Specification field value) = show field ++ ":" ++ value

readSpecification :: ReadP Specification
readSpecification = do
  field <- readField
  _     <- char ':'
  value <- munch1 (not . isSpace)
  return (Specification field value)

instance Read Specification where
  readPrec = readP_to_Prec (const readSpecification)

newtype Passport = Passport [Specification]

instance Show Passport where
  show (Passport specs) = unwords (map show specs)

readPassport :: ReadP Passport
readPassport = do
  specs <- sepBy1 readSpecification (char '\n' <++ char ' ')
  return (Passport specs)

instance Read Passport where
  readPrec = readP_to_Prec (const readPassport)

newtype Passports = Passports [Passport]

instance Show Passports where
  show (Passports passports) = unlines (map show passports)

many2 :: ReadP a -> ReadP [a]
many2 p = liftM2 (:) p (many1 p)

readPassports :: ReadP Passports
readPassports = do
  passports <- sepBy1 readPassport (many2 (char '\n'))
  return (Passports passports)

instance Read Passports where
  readPrec = readP_to_Prec (const readPassports)

readInputFile :: String -> IO [Passport]
readInputFile fileName = do
  content <- readFile fileName
  let Passports passports = read content in
    return passports

main :: IO ()
main = do
  passports <- readInputFile sourceFile
  putStrLn ("Counted " ++ show (length passports) ++ " passports.")
