{-# LANGUAGE ScopedTypeVariables #-}

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

-- It's nicer to pass coordinate pairs around than separate values. This way,
-- it is always clear which is x and which is y.
type Coord = (Int, Int)

data Grid a = Grid { height :: Int
                   , width  :: Int
                   , cells  :: [a] }
  deriving (Eq)

instance Show a => Show (Grid a) where
  show g = intercalate "\n" (map (concatMap show) (gridRows (cells g)))
    where
      gridRows :: [a] -> [[a]]
      gridRows [] = []
      gridRows cs = row : gridRows cs'
        where
          (row, cs') = splitAt (width g) cs

instance Functor Grid where
  fmap f grid = grid{cells=map f (cells grid)}

instance Foldable Grid where
  foldr f z grid = foldr f z (cells grid)

-- Derives the valid width of the grid's rows. If there is no valid width (i.e.,
-- some rows are not the same length as the first row), an error is thrown.
validWidth :: [[a]] -> Int
validWidth [] = 0
validWidth (firstRow:rows) = shapeValid' (length firstRow) rows
  where
    shapeValid' :: Int -> [[a]] -> Int
    shapeValid' w [] = w
    shapeValid' w (row:rows')
      | length row == w = shapeValid' w rows'
      | otherwise       = error "inconsistent row widths"

-- Constructs a grid from a list of rows.
makeGrid :: [[a]] -> Grid a
makeGrid rows = Grid { height = h
                     , width  = w
                     , cells  = concat rows }
  where
    h = length rows
    w = validWidth rows

-- Converts a grid to its list of coordinates.
coords :: Grid a -> [Coord]
coords g = [(x, y) | y <- [0..height g - 1]
                   , x <- [0..width  g - 1]]

-- Constructs a new grid consisting of pairs of the old values and their
-- coordinates within the grid. Useful for debugging.
gridWithCoords :: Grid a -> Grid (Coord, a)
gridWithCoords g = g{cells=zip (coords g) (cells g)}

-- Gets a cell out of a grid by its coordinate location. Throws an error if the
-- coordinate is invalid.
getCell :: Grid a -> Coord -> a
getCell g c = cells g !! coordToIndex c
  where
    -- Converts a coordinate to an index into the grid's list of cells.
    coordToIndex :: Coord -> Int
    coordToIndex (x, y) = x + y * width g

-- Determines whether a given coordinate exists in the grid. It's useful to use
-- this prior to making calls to `getCell` so you don't get errors.
validCoord :: Grid a -> Coord -> Bool
validCoord g (x, y)
  | x < 0 || x >= width  g = False
  | y < 0 || y >= height g = False
  | otherwise              = True

-- Constructs a list of the neighbors of a given cell by coordinate, excluding
-- the original cell. This is a list of the eight cells surrounding the initial
-- cell, but accounts for cells at the borders of the grid as well.
getNeighbors :: forall a. Grid a -> Coord -> [a]
getNeighbors g (x, y) = filteredNeighbors
  where
    filteredNeighbors :: [a]
    filteredNeighbors = map (getCell g) filteredCoords
    filteredCoords :: [Coord]
    filteredCoords = filter validNeighborCoord neighborCoords
    neighborCoords :: [Coord]
    neighborCoords = [(x', y') | y' <- map (+ y) [-1..1]
                               , x' <- map (+ x) [-1..1]]
    validNeighborCoord :: Coord -> Bool
    validNeighborCoord c = validCoord g c && c /= (x, y)

-- Updates the seats of a grid by finding a fixed point.
-- NOTE: This function also emits output as an unsafe side-effect. This is
--       because the algorithm is not very fast and it can appear stuck, even
--       when progress is being made. I'm sorry.
updateSeats :: Grid CellState -> Grid CellState
updateSeats = updateSeats' 0
  where
    -- Computes a single step of the grid and determines whether to make another
    -- recursion (based on whether the new grid is identical to the old one).
    updateSeats' :: Int -> Grid CellState -> Grid CellState
    updateSeats' n g
      | updatedGrid /= g = updateSeats' (n + 1) updatedGrid
      | otherwise        = updatedGrid
      where
        -- Computes a single-step update of the grid, and may also emit output
        -- of how many steps we've taken so far.
        updatedGrid :: Grid CellState
        updatedGrid = showIterations (updateSeatsStep g)
        -- Unsafely shows how many steps we've taken so far.
        showIterations :: a -> a
        showIterations x = if mod n 10 == 0 then trace ("Recursive iteration step: " ++ show n) x else x

-- Updates all the seats in a grid exactly once. The rules are detailed in the
-- instructions, but briefly they are:
--   * If a seat is empty and has no occupied neighbors, it becomes occupied.
--   * If a seat is occupied and has 4+ occupied neighbors, it becomes empty.
--   * Otherwise, there is no change.
updateSeatsStep :: Grid CellState -> Grid CellState
updateSeatsStep g = updatedGrid
  where
    updatedGrid :: Grid CellState
    updatedGrid = g{cells=fixSeats (coords g)}
    fixSeats :: [Coord] -> [CellState]
    fixSeats [] = []
    fixSeats (c:cs) = updatedSeat : fixSeats cs
      where
        updatedSeat :: CellState
        updatedSeat
          | oldSeat == Empty    && notElem Occupied oldNeighbors                   = Occupied
          | oldSeat == Occupied && length (filter (== Occupied) oldNeighbors) >= 4 = Empty
          | otherwise = oldSeat
        oldSeat :: CellState
        oldSeat = getCell g c
        oldNeighbors :: [CellState]
        oldNeighbors = getNeighbors g c

readInputFile :: String -> IO (Grid CellState)
readInputFile fileName = do
  content <- readFile fileName
  return (makeGrid (map read (lines content)))

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  grid <- readInputFile sourceFile
  let updatedGrid = updateSeats grid
      occupiedSeats = length (filter (== Occupied) (cells updatedGrid))
  putStrLn ("\nNumber of seats occupied at the end: " ++ show occupiedSeats)
