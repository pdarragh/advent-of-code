import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Char (isDigit, toLower)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy, skipSpaces, string)
import Text.Read (readListPrec, readPrec, readP_to_Prec)

data InstructionType
  = Acc
  | Jmp
  | Nop
  deriving (Eq, Show)

readInstructionType :: ReadP InstructionType
readInstructionType = Acc <$ string "acc" <|>
                      Jmp <$ string "jmp" <|>
                      Nop <$ string "nop"

instance Read InstructionType where
  readPrec = readP_to_Prec (const readInstructionType)

newtype InstructionValue = InstructionValue Int deriving (Eq, Show)

readInstructionValue :: ReadP InstructionValue
readInstructionValue = do
  sign <- char '+' $>   1  <|>
          char '-' $> (-1)
  rawNum <- munch1 isDigit
  let num = (read :: String -> Int) rawNum
  return (InstructionValue (sign * num))

instance Read InstructionValue where
  readPrec = readP_to_Prec (const readInstructionValue)

data Instruction = Instruction InstructionType InstructionValue

instance Show Instruction where
  show (Instruction t (InstructionValue v)) = map toLower (show t) ++ " " ++ bool "-" "+" (v < 0) ++ show v

readInstruction :: ReadP Instruction
readInstruction = Instruction <$> readInstructionType <* skipSpaces <*> readInstructionValue

readInstructions :: ReadP [Instruction]
readInstructions = sepBy readInstruction skipSpaces

instance Read Instruction where
  readPrec = readP_to_Prec (const readInstruction)
  readListPrec = readP_to_Prec (const readInstructions)

readInputFile :: String -> IO [Instruction]
readInputFile fileName = do
  content <- readFile fileName
  return (read content)

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  instructions <- readInputFile sourceFile
  mapM_ print instructions
