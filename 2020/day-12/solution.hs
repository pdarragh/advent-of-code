-- Hide the Left and Right constructors for Either, since they clash with our
-- Direction type.
import Prelude hiding (Left, Right)

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy, skipSpaces)
import Text.Read (readPrec, readP_to_Prec, readListPrec)

data CardinalDirection = North | South | East | West deriving (Eq)

instance Show CardinalDirection where
  show North = "N"
  show South = "S"
  show East  = "E"
  show West  = "W"

readCardinalDirection :: ReadP CardinalDirection
readCardinalDirection = North <$ char 'N' <|>
                        South <$ char 'S' <|>
                        East  <$ char 'E' <|>
                        West  <$ char 'W'

instance Read CardinalDirection where
  readPrec = readP_to_Prec (const readCardinalDirection)

data RotationalDirection = Left | Right deriving (Eq)

instance Show RotationalDirection where
  show Left  = "L"
  show Right = "R"

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

--
-- Location-Based Figuring
--

data ShipState = ShipState { facing :: CardinalDirection
                           , x      :: Value
                           , y      :: Value
                           }
  deriving (Eq, Show)

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

--
-- Waypoint-Based Figuring
--

data WaypointState = WaypointState { shipX :: Value
                                   , shipY :: Value
                                   , wayX :: Value
                                   , wayY :: Value
                                   }
  deriving (Eq, Show)

initialState :: WaypointState
initialState = WaypointState 0 0 10 1

translateWaypoint :: WaypointState -> CardinalDirection -> Value -> WaypointState
translateWaypoint s North v = s { wayY = wayY s + v }
translateWaypoint s East v = s { wayX = wayX s + v }
translateWaypoint s South v = s { wayY = wayY s - v }
translateWaypoint s West v = s { wayX = wayX s - v }

rotateWaypoint :: WaypointState -> RotationalDirection -> Value -> WaypointState
rotateWaypoint s _ 0 = s
rotateWaypoint s Right d =
  rotateWaypoint (s { wayX = wayY s, wayY = -(wayX s) }) Right (d - 90)
rotateWaypoint s Left d =
  rotateWaypoint (s { wayX = -(wayY s), wayY = wayX s }) Left (d - 90)

translateShipTowardsWaypoint :: WaypointState -> Int -> WaypointState
translateShipTowardsWaypoint s 0 = s
translateShipTowardsWaypoint s i =
  translateShipTowardsWaypoint (s { shipX = shipX s + wayX s
                                  , shipY = shipY s + wayY s
                                  }) (i - 1)

updateState :: WaypointState -> Command -> WaypointState
updateState s (Command (Cardinal cd, v)) = translateWaypoint s cd v
updateState s (Command (Rotational rd, v)) = rotateWaypoint s rd v
updateState s (Command (Forward, v)) = translateShipTowardsWaypoint s v

--
-- Common
--

manhattanDistance :: Value -> Value -> Value
manhattanDistance x y = abs x + abs y

readInputFile :: String -> IO [Command]
readInputFile fileName = do
  content <- readFile fileName
  return (read content)

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  instructions <- readInputFile sourceFile
  let finalShip1 = foldl updateShip initialShip instructions
      distance1 = manhattanDistance (x finalShip1) (y finalShip1)
  putStrLn ("Part 1 final ship state: " ++ show finalShip1)
  putStrLn ("Part 1 Manhattan distance of final ship position from origin: " ++ show distance1)
  let finalShip2 = foldl updateState initialState instructions
      distance2 = manhattanDistance (shipX finalShip2) (shipY finalShip2)
  putStrLn ("Part 2 final ship state: " ++ show finalShip2)
  putStrLn ("Part 2 Manhattan distance of final ship position from origin: " ++ show distance2)
