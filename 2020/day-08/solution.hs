{-# LANGUAGE MultiWayIf #-}

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Bool (bool)
import Data.Char (isDigit, toLower)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy, skipSpaces, string)
import Text.Read (readListPrec, readPrec, readP_to_Prec)

-- Updates an element in a list by applying a function to it. Throws an error if
-- the given index is out of the bounds of the list.
updateWith :: (a -> a) -> Int -> [a] -> [a]
updateWith xf idx xs
  | idx < 0 || idx >= length xs =
    error "index out of bounds"
  | otherwise =
    take idx xs ++ [xf (xs !! idx)] ++ drop (idx + 1) xs

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
  show (Instruction t (InstructionValue v)) = map toLower (show t) ++ " " ++ bool "+" "" (v < 0) ++ show v

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
initializeIPU :: State IPUState a -> a
initializeIPU = fst . flip runState initialState

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

-- Runs a given set of instructions, halting if a loop is encountered.
--
-- The return value is a pair where the first element is the order of executed
-- instruction pointers (constituting a rudimentary form of trace), and the
-- second element is the final accumulated value in the accumulation register.
-- However, that accumulated value is wrapped in either a Left (indicating the
-- execution terminated successfully by incriminating past the final instruction)
-- or a Right (indicating execution was halted because either the instruction
-- pointer was set to a value outside the list of instructions, or else a loop
-- was detected and terminated).
runInstructions :: [Instruction] -> State IPUState ([Int], Either Int Int)
runInstructions instructions = runInstructions' initialLoopMap [] >>= finalizeResult
  where
    initialLoopMap :: [Bool]
    initialLoopMap = replicate (length instructions) False
    runInstructions' :: [Bool] -> [Int] -> State IPUState ([Int], Int -> Either Int Int)
    runInstructions' alreadyRun execOrder = do
      ip <- currentIp
      if
        | ip == length instructions          -> return (reverse execOrder, Left)
        | ip < 0 || ip > length instructions -> return (reverse execOrder, Right)
        | alreadyRun !! ip                   -> return (reverse execOrder, Right)
        | otherwise                          -> executeInstruction (instructions !! ip)
                                                >> runInstructions' (markAsRun ip alreadyRun) (ip : execOrder)
    markAsRun :: Int -> [Bool] -> [Bool]
    markAsRun = updateWith (const True)
    finalizeResult :: ([Int], Int -> Either Int Int) -> State IPUState ([Int], Either Int Int)
    finalizeResult (l, rf) = do
      acc <- currentAcc
      return (l, rf acc)

-- Assumes a given set of instructions has a single fault and attempts to fix
-- that fault, returning a triple if the fault is found and fixed.
--
-- The return value is either Nothing (the fault could not be found) or else
-- a triple consisting of:
--   1. The index of the faulty instruction that was fixed.
--   2. The execution trace of the instructions.
--   3. The final value in the accumulation register.
--
-- It is assumed that any fault in the instructions lies within either a Jmp
-- that should be a Nop, or a Nop that should be a Jmp. This function is naive
-- and simply iterates over each of the Jmp/Nop instructions and swaps it.
--
-- NOTE: This solution is embarrassingly parallel and could be sped up
--       considerably by naive threading, but that sounds like too much for me.
swapJmpsAndNops :: [Instruction] -> Maybe (Int, [Int], Int)
swapJmpsAndNops instructions = swapJmpsAndNops' ipsToSwap
  where
    ipsToSwap :: [Int]
    ipsToSwap = map fst (filter (isJmpOrNop . snd) (zip [0..] instructions))
    isJmpOrNop :: Instruction -> Bool
    isJmpOrNop (Instruction t _)
      | t == Jmp || t == Nop = True
      | otherwise            = False
    swapJmpsAndNops' :: [Int] -> Maybe (Int, [Int], Int)
    swapJmpsAndNops' [] = Nothing
    swapJmpsAndNops' (ip:ips) =
      let result = initializeIPU (runInstructions (swapInstruction ip)) in
      case snd result of
        Left acc -> Just (ip, fst result, acc)
        Right _  -> swapJmpsAndNops' ips
    swapInstruction :: Int -> [Instruction]
    swapInstruction ip = updateWith swapInstruction' ip instructions
    swapInstruction' :: Instruction -> Instruction
    swapInstruction' (Instruction t v) = Instruction t' v
      where
        t' = case t of
          Jmp -> Nop
          Nop -> Jmp
          _   -> t

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
  -- Part 1 output.
  let initialAcc = initializeIPU (runInstructions instructions)
  case snd initialAcc of
    Left acc  -> putStrLn ("Accumulator value after termination without replacements: " ++ show acc)
    Right acc -> putStrLn ("Accumulator value after stopping an infinite loop: " ++ show acc)
  -- Part 2 output.
  let secondAcc = swapJmpsAndNops instructions
  case secondAcc of
    Nothing  -> putStrLn "Was unable to find an instruction to modify to prevent infinite loop."
    Just (_, _, acc) -> putStrLn ("Accumulator value after modifying an instruction: " ++ show acc)
