import Control.Monad (liftM2, replicateM)
import Data.Char (isSpace)
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP (ReadP, (<++), char, get, many1, munch1, pfail, sepBy1)
import Text.Read (readPrec, readP_to_Prec)

sourceFile :: String
sourceFile = "input.txt"

-- Only these fields are valid in passports.
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

-- We expect passport fields to be labeled by the following 3-letter codes.
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

-- Sometimes we want to lookup in `fieldLabels` by code instead of field.
reverseLookup :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookup key pairs = lookup key (map swap pairs)

-- Like `ReadP.get` but gathers n characters into a string.
getN :: Int -> ReadP String
getN n = replicateM n get

-- Like `ReadP.many` but requires at least 2 occurrences of *p*.
many2 :: ReadP a -> ReadP [a]
many2 p = liftM2 (:) p (many1 p)

-- Defines a parser for reading a field. Only the defined fields are supported.
readField :: ReadP Field
readField = do
  label <- getN 3
  case reverseLookup label fieldLabels of
    Nothing    -> pfail
    Just field -> return field

instance Read Field where
  -- Reads a field from a string.
  readPrec = readP_to_Prec (const readField)

-- A specification is a pair of a field and a value.
--
-- NOTE: Normally I would just use a tuple for this but I wanted a `read`-able
--       type instead.
data Specification = Specification { getField :: Field
                                   , getValue :: String }

-- Defines a parser for reading a specification. A specification consists of a
-- field code followed by a colon and then a string of non-space characters.
readSpecification :: ReadP Specification
readSpecification = do
  field <- readField
  _     <- char ':'
  value <- munch1 (not . isSpace)
  return (Specification field value)

instance Read Specification where
  -- Reads a specification from a string.
  readPrec = readP_to_Prec (const readSpecification)

-- A passport is just a collection of specifications.
newtype Passport = Passport [Specification]

-- Defines a parser for a passport. Specifications can be separated by spaces or
-- newlines, but only single newlines are allowed.
readPassport :: ReadP Passport
readPassport = do
  specs <- sepBy1 readSpecification (char '\n' <++ char ' ')
  return (Passport specs)

instance Read Passport where
  -- Reads a passport from a string.
  readPrec = readP_to_Prec (const readPassport)

-- A collection of passports. This exists specifically to enable `read`ing.
newtype Passports = Passports [Passport]

-- Defines a parser for reading passports. Consecutive passports are separated
-- by one or more blank lines (two or more consecutive newline characters).
readPassports :: ReadP Passports
readPassports = do
  passports <- sepBy1 readPassport (many2 (char '\n'))
  return (Passports passports)

instance Read Passports where
  -- Reads a collection of passports from a string.
  readPrec = readP_to_Prec (const readPassports)

-- These fields *must* appear in a valid passport.
requiredFields :: [Field]
requiredFields =
  [ BirthYear
  , IssueYear
  , ExpirationYear
  , Height
  , HairColor
  , EyeColor
  , PassportID
  ]

-- Simple (part 1) passport validation checks only whether all required fields
-- are present.
isPassportValid :: Passport -> Bool
isPassportValid (Passport specifications) =
  all (`elem` map getField specifications) requiredFields

-- Converts a file into a list of passports.
readInputFile :: String -> IO [Passport]
readInputFile fileName = do
  content <- readFile fileName
  let Passports passports = read content in
    return passports

main :: IO ()
main = do
  passports <- readInputFile sourceFile
  putStrLn ("Counted " ++ show (length (filter isPassportValid passports)) ++ " valid passports.")
