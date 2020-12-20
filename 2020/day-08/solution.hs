{-# LANGUAGE MultiWayIf, ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Bool (bool)
import Data.Char (isDigit, toLower)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy, skipSpaces, string)
import Text.Read (readListPrec, readPrec, readP_to_Prec)

-- Describes the instruction types.
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

-- A newtype is used so we can define a ReadP instance.
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

-- An instruction just pairs an instruction type with a value. All instructions
-- have values (even nop).
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

-- IPU = Instruction Processing Unit, because I am very clever.
data IPUState = IPUState { ipuAcc :: Int   -- Accumulator register.
                         , ipuIp  :: Int } -- Instruction Pointer register.

-- The initial state is just zeroed-out registers.
initialState :: IPUState
initialState = IPUState 0 0

-- Starts up the IPU with zeroed-out registers.
initializeIPU :: State IPUState a -> (a, IPUState)
initializeIPU = flip runState initialState

currentAcc :: State IPUState Int
currentAcc = gets ipuAcc

currentIp :: State IPUState Int
currentIp = gets ipuIp

-- Updates the accumulator register, adding the given value to the existing.
updateAcc :: Int -> State IPUState ()
updateAcc newAcc = get >>= \s -> put s { ipuAcc = newAcc + ipuAcc s }

updateIp :: Int -> State IPUState ()
updateIp newIp = get >>= \s -> put s { ipuIp = newIp }

updateIpByOffset :: Int -> State IPUState ()
updateIpByOffset off = (+ off) <$> currentIp >>= updateIp

incrementIp :: State IPUState ()
incrementIp = updateIpByOffset 1

-- The execution of any single instruction is straightforward. Acc values add to
-- the accumulator and move forward; Jmp values jump to a given instruction; and
-- Nop values do nothing, but move to the next instruction.
executeInstruction :: Instruction -> State IPUState ()
executeInstruction (Instruction t (InstructionValue v)) = case t of
  Acc -> updateAcc v >> incrementIp
  Jmp -> updateIpByOffset v
  Nop -> incrementIp

runInstructions :: forall a. [Instruction] -> State IPUState a -> State IPUState (Either Int a)
runInstructions instructions sentinelOnLoop = runInstructions' initialLoopMap
  where
    initialLoopMap :: [Bool]
    initialLoopMap = replicate (length instructions) False
    runInstructions' :: [Bool] -> State IPUState (Either Int a)
    runInstructions' alreadyRun = do
      ip <- currentIp
      if
        | ip > length instructions -> Left <$> currentAcc
        | alreadyRun !! ip         -> Right <$> sentinelOnLoop
        | otherwise                -> runInstruction ip >> runInstructions' (markAsRun ip alreadyRun)
    runInstruction :: Int -> State IPUState ()
    runInstruction ip = executeInstruction (instructions !! ip)
    markAsRun :: Int -> [Bool] -> [Bool]
    markAsRun ip alreadyRun = take (ip - 1) alreadyRun ++ [True] ++ drop ip alreadyRun

-- Converts a file to a list of instructions to execute.
readInputFile :: String -> IO [Instruction]
readInputFile fileName = do
  content <- readFile fileName
  return (read content)

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  instructions <- readInputFile sourceFile
  let Right acc = fst (initializeIPU (runInstructions instructions currentAcc))
  putStrLn ("Accumulator value after final non-looping instruction: " ++ show acc)
