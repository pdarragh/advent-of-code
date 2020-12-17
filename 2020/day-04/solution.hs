import Control.Monad (liftM2, replicateM)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
  ( ReadP, readP_to_S, (<++)
  , char, choice, get, many1, munch1, pfail, satisfy, sepBy1, string
  )
import Text.Read (readPrec, readP_to_Prec)

-- Like `ReadP.get` but gathers n characters into a string.
getN :: Int -> ReadP String
getN n = replicateM n get

-- Like `ReadP.many` but requires at least 2 occurrences of *p*.
many2 :: ReadP a -> ReadP [a]
many2 p = liftM2 (:) p (many1 p)

-- Run a parser without a Read instance. The `readP_to_S` function used returns
-- a list. We consider the parser successful only if there exists exactly one
-- pair in the list whose second element (the un-parsed remaining string)
-- consists entirely of whitespace or newline characters.
runP_to_a :: ReadP a -> String -> Maybe a
runP_to_a p s = case results of
  [(result, "")] -> Just result
  _              -> Nothing
  where
    results = filter (all isSpace . snd) (readP_to_S p s)

-- Runs a parser and returns True if the parse succeeded, otherwise False.
runP_to_Bool :: ReadP a -> String -> Bool
runP_to_Bool p s = boolFromMaybe (True <$ runP_to_a p s)

-- Runs a parser and, if the parse was successful, validates the result using
-- the supplied function.
validateP_as_Bool :: ReadP a -> (a -> Bool) -> String -> Bool
validateP_as_Bool p f s = boolFromMaybe (f <$> runP_to_a p s)

-- Minimizes a trinary Maybe Bool to a binary Bool.
boolFromMaybe :: Maybe Bool -> Bool
boolFromMaybe Nothing = False
boolFromMaybe (Just b) = b

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

instance Show Field where
  -- Renders a field as its label.
  show f = fromJust (lookup f fieldLabels)

-- Sometimes we want to lookup in `fieldLabels` by code instead of field.
reverseLookup :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookup key pairs = lookup key (map swap pairs)

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
                     deriving (Eq)

instance Show Specification where
  -- Renders a specification as the field label with its value, separated by a
  -- colon.
  show s = show (getField s) ++ ":" ++ getValue s

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
newtype Passport = Passport [Specification] deriving (Eq)

instance Show Passport where
  -- Renders a passport as its specifications, each on its own line.
  show (Passport specs) = unlines (map show specs)

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
newtype Passports = Passports [Passport] deriving (Eq)

instance Show Passports where
  -- Renders a collection of passports each separated by a blank line.
  show (Passports ps) = intercalate "\n\n" (map show ps)

-- Defines a parser for reading passports. Consecutive passports are separated
-- by one or more blank lines (two or more consecutive newline characters).
readPassports :: ReadP Passports
readPassports = do
  passports <- sepBy1 readPassport (many2 (char '\n'))
  return (Passports passports)

instance Read Passports where
  -- Reads a collection of passports from a string.
  readPrec = readP_to_Prec (const readPassports)

-- Defines the bounds for the supported height units.
heightBounds :: String -> (Int, Int)
heightBounds "cm" = (150, 193)
heightBounds "in" = (59, 76)
heightBounds s    = error ("invalid height unit: '" ++ s ++ "'")

-- Reads a height from a string, producing a pair consisting of the unit and
-- the value. The value is an un-validated non-negative integer.
readHeight :: ReadP (String, Int)
readHeight = do
  num  <- munch1 isDigit
  unit <- choice [string "cm", string "in"]
  return (unit, read num)

-- Defines a validation function for each required field. These are run by
-- providing the field value as a string. Many of them are just parsers.
requiredFieldValidators :: [(Field, String -> Bool)]
requiredFieldValidators =
  [ (BirthYear,      intBound 1920 2002 . read)
  , (IssueYear,      intBound 2010 2020 . read)
  , (ExpirationYear, intBound 2020 2030 . read)
  , (Height,         validateP_as_Bool readHeight (\(unit, val) -> uncurry intBound (heightBounds unit) val))
  , (HairColor,      runP_to_Bool (char '#' >> replicateM 6 (satisfy (\c -> isDigit c || elem c "abcdef"))))
  , (EyeColor,       runP_to_Bool (choice (map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])))
  , (PassportID,     runP_to_Bool (replicateM 9 (satisfy isDigit)))
  ] where
  intBound :: Int -> Int -> Int -> Bool
  intBound lo hi val = val >= lo && val <= hi

-- Simple (part 1) passport validation checks only whether all required fields
-- are present.
passportContainsRequiredFields :: Passport -> Bool
passportContainsRequiredFields (Passport specifications) =
  all (`elem` map getField specifications) (map fst requiredFieldValidators)

-- More complex (part 2) passport validation. After checking whether all
-- required fields are present, validates those fields' values.
passportFieldsHaveValidValues :: Passport -> Bool
passportFieldsHaveValidValues p@(Passport specifications) =
  passportContainsRequiredFields p &&
  all isSpecValid specifications where
    isSpecValid :: Specification -> Bool
    isSpecValid spec = case lookup (getField spec) requiredFieldValidators of
      Nothing       -> True
      Just validate -> validate (getValue spec)

-- Converts a file into a list of passports.
readInputFile :: String -> IO [Passport]
readInputFile fileName = do
  content <- readFile fileName
  let Passports passports = read content in
    return passports

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  passports <- readInputFile sourceFile
  putStrLn (show (length (filter passportContainsRequiredFields passports)) ++ " passports contain all required fields.")
  putStrLn (show (length (filter passportFieldsHaveValidValues passports)) ++ " of those passports are completely valid.")
