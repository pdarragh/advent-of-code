import Data.Char (isAscii, isLower)
import Data.List (intersect, nub)
import Text.ParserCombinators.ReadP
import Text.Read (readPrec, readListPrec, readP_to_Prec)

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

-- Counts the number of questions to which anybody in the group answered "yes".
countAnyGroupAnswers :: GroupAnswers -> Int
countAnyGroupAnswers (GroupAnswers answers) = length (nub (concat answers))

-- Sums the counts of "yes" answers by anyone in each group.
sumAnyGroupAnswerCounts :: [GroupAnswers] -> Int
sumAnyGroupAnswerCounts = sum . map countAnyGroupAnswers

-- Counts the number of questions to which everyone in the group answered "yes".
countEveryGroupAnswers :: GroupAnswers -> Int
countEveryGroupAnswers (GroupAnswers answers) = length (foldr intersect (head answers) (tail answers))

-- Sums the counts of "yes" answers by everyone in each group.
sumEveryGroupAnswerCounts :: [GroupAnswers] -> Int
sumEveryGroupAnswerCounts = sum . map countEveryGroupAnswers

-- Converts a file into a list of groups' answers.
readInputFile :: String -> IO [GroupAnswers]
readInputFile fileName = do
  content <- readFile fileName
  return (read content)

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  groupAnswers <- readInputFile sourceFile
  let anyAnswerSum = sumAnyGroupAnswerCounts groupAnswers
  putStrLn ("The sum of the number of questions answered 'yes' by anyone in all groups is: " ++ show anyAnswerSum ++ ".")
  let everyAnswerSum = sumEveryGroupAnswerCounts groupAnswers
  putStrLn ("The sum of the number of questions answered 'yes' by everyone in all groups is: " ++ show everyAnswerSum ++ ".")
