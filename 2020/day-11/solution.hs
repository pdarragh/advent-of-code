import Control.Applicative ((<|>))
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP, char, many1)
import Text.Read (readListPrec, readPrec, readP_to_Prec)

import Debug.Trace (trace)

data CellState
  = Floor
  | Empty
  | Occupied
  deriving (Eq)

instance Show CellState where
  show Floor    = "."
  show Empty    = "L"
  show Occupied = "#"

readCellState :: ReadP CellState
readCellState = Floor    <$ char '.' <|>
                Empty    <$ char 'L' <|>
                Occupied <$ char '#'

readCellStates :: ReadP [CellState]
readCellStates = many1 readCellState

instance Read CellState where
  readPrec = readP_to_Prec (const readCellState)
  readListPrec = readP_to_Prec (const readCellStates)

type Coord = (Int, Int)

data Grid = Grid { height :: Int
                 , width  :: Int
                 , cells  :: [CellState] }
  deriving (Eq)

instance Show Grid where
  show g = intercalate "\n" (map (concatMap show) (gridRows (cells g)))
    where
      gridRows :: [CellState] -> [[CellState]]
      gridRows [] = []
      gridRows cs = row : gridRows cs'
        where
          (row, cs') = splitAt (width g) cs

validWidth :: [[CellState]] -> Int
validWidth [] = 0
validWidth (firstRow:rows) = shapeValid' (length firstRow) rows
  where
    shapeValid' :: Int -> [[CellState]] -> Int
    shapeValid' w [] = w
    shapeValid' w (row:rows')
      | length row == w = shapeValid' w rows'
      | otherwise       = error "inconsistent row widths"

makeGrid :: [[CellState]] -> Grid
makeGrid rows = Grid { height = h
                     , width  = w
                     , cells  = concat rows }
  where
    h = length rows
    w = validWidth rows

coords :: Grid -> [Coord]
coords g = [(x, y) | y <- [0..height g - 1]
                   , x <- [0..width  g - 1]]

getCell :: Grid -> Coord -> CellState
getCell g c = cells g !! coordToIndex g c

coordToIndex :: Grid -> Coord -> Int
coordToIndex g (x, y) = y + x * width g

validCoord :: Grid -> Coord -> Bool
validCoord g (x, y)
  | x < 0 || x >= width  g = False
  | y < 0 || y >= height g = False
  | otherwise              = True

getNeighbors :: Grid -> Coord -> [CellState]
getNeighbors g (x, y) = filteredNeighbors
  where
    filteredNeighbors :: [CellState]
    filteredNeighbors = map (cells g !!) filteredIndices
    filteredIndices :: [Int]
    filteredIndices = map (coordToIndex g) filteredCoords
    filteredCoords :: [Coord]
    filteredCoords = filter validNeighborCoord neighborCoords
    neighborCoords :: [Coord]
    neighborCoords = [(x', y') | y' <- map (+ y) [-1..1]
                               , x' <- map (+ x) [-1..1]]
    validNeighborCoord :: Coord -> Bool
    validNeighborCoord c = validCoord g c && c /= (x, y)

updateSeats :: Grid -> Grid
updateSeats g
  | updatedGrid /= g = trace ("updated grid:\n" ++ show updatedGrid ++ "\n\n") updateSeats updatedGrid
  | otherwise        = updatedGrid
  where
    updatedGrid :: Grid
    updatedGrid = g{cells=fixSeats (coords g)}
    fixSeats :: [Coord] -> [CellState]
    fixSeats [] = []
    fixSeats (c:cs) = updatedSeat : fixSeats cs
      where
        updatedSeat :: CellState
        updatedSeat
          | oldSeat == Empty    && notElem Empty oldNeighbors                      = Occupied
          | oldSeat == Occupied && length (filter (== Occupied) oldNeighbors) >= 4 = Empty
          | otherwise = oldSeat
        oldSeat :: CellState
        oldSeat = getCell g c
        oldNeighbors :: [CellState]
        oldNeighbors = getNeighbors g c

readInputFile :: String -> IO Grid
readInputFile fileName = do
  content <- readFile fileName
  return (makeGrid (map read (lines content)))

sourceFile :: String
sourceFile = "input2.txt"

main :: IO ()
main = do
  grid <- readInputFile sourceFile
  let updatedGrid = trace ("original grid:\n" ++ show grid ++ "\n\n") updateSeats grid
      occupiedSeats = length (filter (== Occupied) (cells updatedGrid))
  putStrLn ("Number of seats occupied at the end: " ++ show occupiedSeats)
