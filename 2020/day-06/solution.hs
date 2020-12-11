import Data.Char (isAscii, isLower)
import Text.ParserCombinators.ReadP
import Text.Read (readPrec, readListPrec, readP_to_Prec)

sourceFile :: String
sourceFile = "input.txt"

-- An Answer indicates a "yes" to a particular question, indicated by a letter.
newtype Answer = Answer Char deriving (Eq, Show)

readAnswer :: ReadP Answer
readAnswer = do
  c <- satisfy (\c -> isAscii c && isLower c)
  return (Answer c)

readAnswers :: ReadP [Answer]
readAnswers = many1 readAnswer

instance Read Answer where
  readPrec = readP_to_Prec (const readAnswer)
  readListPrec = readP_to_Prec (const readAnswers)

-- The answers belonging to a group of people.
newtype GroupAnswers = GroupAnswers [[Answer]] deriving (Eq, Show)

readGroupAnswers :: ReadP GroupAnswers
readGroupAnswers = do
  answers <- sepBy1 readAnswers (char '\n')
  return (GroupAnswers answers)

readAllAnswers :: ReadP [GroupAnswers]
readAllAnswers = sepBy1 readGroupAnswers (char '\n' *> many1 (char '\n'))

instance Read GroupAnswers where
  readPrec = readP_to_Prec (const readGroupAnswers)
  readListPrec = readP_to_Prec (const readAllAnswers)

readInputFile :: String -> IO [GroupAnswers]
readInputFile fileName = do
  content <- readFile fileName
  return (read content)

main :: IO ()
main = do
  groupAnswers <- readInputFile sourceFile
  print groupAnswers
