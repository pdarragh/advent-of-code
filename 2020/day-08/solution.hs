import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Bool (bool)
import Data.Char (isDigit, toLower)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy, skipSpaces, string)
import Text.Read (readListPrec, readPrec, readP_to_Prec)

-- Takes two applicative functions expecting an argument and passes the same
-- argument to both, discarding the result returned by the first.
(*>*>) :: Applicative f => (a -> f b) -> (a -> f c) -> (a -> f c)
(*>*>) f1 f2 a = f1 a *> f2 a
infix 4 *>*>

-- Similar to Data.Bool.bool, except each element is a function expecting a
-- value. The same value is given to all three elements.
boolF :: (a -> b) -> (a -> b) -> (a -> Bool) -> a -> b
boolF t f c x = bool (t x) (f x) (c x)

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

-- Executes the given instructions, starting at the top, but stops whenever an
-- instruction is going to be executed for a second time.
runInstructionsWithoutLoop :: [Instruction] -> State IPUState ()
runInstructionsWithoutLoop instructions = runInstructionsWithoutLoop' (replicate (length instructions) True)
  where
    runInstructionsWithoutLoop' :: [Bool] -> State IPUState ()
    runInstructionsWithoutLoop' notYetRun =
      -- NOTE: This is not the best way to write this. It would be more
      --       straightforward to just use do-notation, I think, but I wanted to
      --       play with this a bit.
      currentIp >>= boolF (void . return)
                          (runInstruction *>*> runInstructionsWithoutLoop' . markAsRun notYetRun)
                          (notYetRun !!)
    runInstruction :: Int -> State IPUState ()
    runInstruction ip = executeInstruction (instructions !! ip)
    markAsRun :: [Bool] -> Int -> [Bool]
    markAsRun notYetRun ip = precs ++ [False] ++ succs
      where
        precs = reverse (drop 1 (reverse precs'))
        (precs', succs) = splitAt ip notYetRun

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
  let acc = fst (initializeIPU (runInstructionsWithoutLoop instructions >> currentAcc))
  putStrLn ("Accumulator value after final non-looping instruction: " ++ show acc)
