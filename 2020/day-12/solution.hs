-- Hide the Left and Right constructors for Either, since they clash with our
-- Direction type.
import Prelude hiding (Left, Right)

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy, skipSpaces)
import Text.Read (readPrec, readP_to_Prec, readListPrec)

data CardinalDirection = North | South | East | West deriving (Eq)

instance Show CardinalDirection where
  show North   = "N"
  show South   = "S"
  show East    = "E"
  show West    = "W"

readCardinalDirection :: ReadP CardinalDirection
readCardinalDirection = North <$ char 'N' <|>
                        South <$ char 'S' <|>
                        East  <$ char 'E' <|>
                        West  <$ char 'W'

instance Read CardinalDirection where
  readPrec = readP_to_Prec (const readCardinalDirection)

data RotationalDirection = Left | Right deriving (Eq)

instance Show RotationalDirection where
  show Left    = "L"
  show Right   = "R"

readRotationalDirection :: ReadP RotationalDirection
readRotationalDirection = Left  <$ char 'L' <|>
                          Right <$ char 'R'

instance Read RotationalDirection where
  readPrec = readP_to_Prec (const readRotationalDirection)

data Direction
  = Cardinal CardinalDirection
  | Rotational RotationalDirection
  | Forward
  deriving (Eq)

instance Show Direction where
  show (Cardinal cd) = show cd
  show (Rotational rd) = show rd
  show Forward = "F"

readDirection :: ReadP Direction
readDirection = Cardinal   <$> readCardinalDirection <|>
                Rotational <$> readRotationalDirection <|>
                Forward    <$  char 'F'

instance Read Direction where
  readPrec = readP_to_Prec (const readDirection)

type Value = Int

readValue :: ReadP Value
readValue = do
  digits <- many1 (satisfy isDigit)
  return (read digits)

newtype Command = Command (Direction, Int)

instance Show Command where
  show (Command (d, v)) = "(" ++ show d ++ ", " ++ show v ++ ")"

readCommand :: ReadP Command
readCommand = do
  direction <- readDirection
  value <- readValue
  return (Command (direction, value))

readCommands :: ReadP [Command]
readCommands = sepBy readCommand skipSpaces

instance Read Command where
  readPrec = readP_to_Prec (const readCommand)
  readListPrec = readP_to_Prec (const readCommands)

data ShipState = ShipState { facing :: CardinalDirection
                           , x      :: Value
                           , y      :: Value }
  deriving (Eq)

instance Show ShipState where
  show s = "{ facing = " ++ show (facing s) ++ ", x = " ++ show (x s) ++ ", y = " ++ show (y s) ++ " }"

initialShip :: ShipState
initialShip = ShipState East 0 0

translateShip :: ShipState -> CardinalDirection -> Value -> ShipState
translateShip s North v = s { y = y s + v }
translateShip s South v = s { y = y s - v }
translateShip s East  v = s { x = x s + v }
translateShip s West  v = s { x = x s - v }

cardinals :: [CardinalDirection]
cardinals = [North, East, South, West]

cardinalIndex :: CardinalDirection -> Int
cardinalIndex North = 0
cardinalIndex East  = 1
cardinalIndex South = 2
cardinalIndex West  = 3

rotateShip :: CardinalDirection -> RotationalDirection -> Value -> CardinalDirection
rotateShip cd rd v =
  let baseCycles = v `quot` 90
      cycles = case rd of
                 Left  -> -1 * baseCycles
                 Right -> baseCycles
      shiftedCycles  = cycles + cardinalIndex cd
  in cardinals !! mod shiftedCycles 4

updateShip :: ShipState -> Command -> ShipState
updateShip s (Command (Cardinal cd, v)) =
  translateShip s cd v
updateShip s (Command (Rotational rd, v)) =
  s { facing = rotateShip (facing s) rd v }
updateShip s (Command (Forward, v)) =
  translateShip s (facing s) v

manhattanDistance :: ShipState -> Value
manhattanDistance s = abs (x s) + abs (y s)

readInputFile :: String -> IO [Command]
readInputFile fileName = do
  content <- readFile fileName
  return (read content)

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  instructions <- readInputFile sourceFile
  let finalShip = foldl updateShip initialShip instructions
      distance = manhattanDistance finalShip
  putStrLn ("Final ship state: " ++ show finalShip)
  putStrLn ("Manhattan distance of final ship position from origin: " ++ show distance)
