{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Data.List (find, intercalate)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Text.ParserCombinators.ReadP (ReadP, char, many1)
import Text.Read (readListPrec, readPrec, readP_to_Prec)

import Debug.Trace (trace)

-- The three states a cell can occupy in this problem.
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
gridWithCoords :: Grid a -> Grid (a, Coord)
gridWithCoords g = g{cells=zip (cells g) (coords g)}

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

data Direction = NW | N | NE | W | E | SW | S | SE

allDirections :: [Direction]
allDirections = [NW, N, NE, W, E, SW, S, SE]

-- On this coordinate plane, the top-left is the origin, with the y-axis moving
-- top-to-bottom and the x-axis moving left-to-right. (In a traditional
-- coordinate plane, the origin is in the bottom-left.)
unit :: Direction -> Coord
unit NW = (-1, -1)
unit N  = ( 0, -1)
unit NE = ( 1, -1)
unit W  = (-1,  0)
unit E  = ( 1,  0)
unit SW = (-1,  1)
unit S  = ( 0,  1)
unit SE = ( 1,  1)

scale :: Coord -> Int -> Coord
scale (x, y) s = (s * x, s * y)

-- Constructs an infinite list of steps starting at the given coordinate and
-- traveling in the indicated direction.
steps :: Coord -> Direction -> [Coord]
steps c d = [add c (scale (unit d) i) | i <- [0..]]
  where
    add :: Coord -> Coord -> Coord
    add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Constructs a finite list of all the cells starting at the given coordinate
-- and extending to the edge of the grid in the given direction.
getCellsInDirection :: Grid a -> Coord -> Direction -> [a]
getCellsInDirection g c d = map (getCell g) (takeWhile (validCoord g) (steps c d))

-- Constructs a list of the neighbors of a given cell by coordinate, excluding
-- the original cell. This is a list of the eight cells surrounding the initial
-- cell, but accounts for cells at the borders of the grid as well.
getNeighbors :: Grid a -> Coord -> [a]
getNeighbors g (x, y) = map (getCell g) filteredCoords
  where
    filteredCoords :: [Coord]
    filteredCoords = filter validNeighborCoord neighborCoords
    neighborCoords :: [Coord]
    neighborCoords = [(x', y') | y' <- map (+ y) [-1..1]
                               , x' <- map (+ x) [-1..1]]
    validNeighborCoord :: Coord -> Bool
    validNeighborCoord c = validCoord g c && c /= (x, y)

-- Constructs a list of the neighbors of a given cell according to a predicate.
-- In this context, "neighbors" is applied loosely and means "the closest cell
-- in each direction which satisfies the predicate."
getFirstNeighborsWithPredicate :: forall a. (a -> Bool) -> Grid a -> Coord -> [a]
getFirstNeighborsWithPredicate p g c = mapMaybe (find p) radialCells
  where
    radialCells :: [[a]]
    radialCells = map (drop 1 . getCellsInDirection g c) allDirections

-- A record of how to attempt to perform an update. Modeling updates this way
-- allows us to generalize the update function.
data UpdateRule = UpdateRule { fromState       :: CellState
                             , toState         :: CellState
                             , updateCondition :: Grid CellState -> Coord -> Bool }

-- Updates the seats of a grid by finding a fixed point.
-- NOTE: This function also emits output as an unsafe side-effect. This is
--       because the algorithm is not very fast and it can appear stuck, even
--       when progress is being made. I'm sorry.
updateSeats :: [UpdateRule] -> Grid CellState -> Grid CellState
updateSeats rules = updateSeats' 0
  where
    -- Computes a single step of the grid and determines whether to make another
    -- recursion (based on whether the new grid is identical to the old one).
    updateSeats' :: Int -> Grid CellState -> Grid CellState
    updateSeats' n g
      | updatedGrid /= g = updateSeats' (n + 1) updatedGrid
      | otherwise        = trace ("Total recursive iteration steps: " ++ show n) updatedGrid
      where
        -- Computes a single-step update of the grid, and may also emit output
        -- of how many steps we've taken so far.
        updatedGrid :: Grid CellState
        updatedGrid = showIterations (updateSeatsStep rules g)
        -- Unsafely shows how many steps we've taken so far.
        showIterations :: a -> a
        showIterations x = if mod n 10 == 0 then trace ("Recursive iteration step: " ++ show n) x else x


-- Updates all the seats in a grid exactly once.
updateSeatsStep :: [UpdateRule] -> Grid CellState -> Grid CellState
updateSeatsStep rules g = updatedGrid
  where
    updatedGrid :: Grid CellState
    updatedGrid = fmap (uncurry updateSeat) coordGrid
    coordGrid :: Grid (CellState, Coord)
    coordGrid = gridWithCoords g
    updateSeat :: CellState -> Coord -> CellState
    updateSeat oldState c = case filter isJust (map (attemptUpdate oldState c) rules) of
      []           -> oldState
      (newState:_) -> fromJust newState
    attemptUpdate :: CellState -> Coord -> UpdateRule -> Maybe CellState
    attemptUpdate oldState c UpdateRule{..}
      | fromState == oldState
        && updateCondition g c = Just toState
      | otherwise              = Nothing

-- These are the rules that updates follow for Part 1's instructions. They are:
--   * If a seat is empty and has no occupied neighbors, it becomes occupied.
--   * If a seat is occupied and has 4+ occupied neighbors, it becomes empty.
part1Rules :: [UpdateRule]
part1Rules = [ UpdateRule Empty Occupied ((notElem Occupied .) . getNeighbors)
             , UpdateRule Occupied Empty ((((>= 4) . length . filter (== Occupied)) .) . getNeighbors) ]

-- These are the rules that updates follow for Part 2's instructions. They are:
--   * If a seat is empty and has no occupied neighbors, it becomes occupied.
--   * If a seat is occupied and has 5+ occupied neighbors, it becomes empty.
part2Rules :: [UpdateRule]
part2Rules = [ UpdateRule Empty Occupied ((notElem Occupied .) . getFirstNeighborsWithPredicate (/= Floor))
             , UpdateRule Occupied Empty ((((>= 5) . length . filter (== Occupied)) .) . getFirstNeighborsWithPredicate (/= Floor)) ]

readInputFile :: String -> IO (Grid CellState)
readInputFile fileName = do
  content <- readFile fileName
  return (makeGrid (map read (lines content)))

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  grid <- readInputFile sourceFile
  putStrLn "Calculating seats occupied using fixed point of applying part 1 rules..."
  let part1UpdatedGrid = updateSeats part1Rules grid
      occupiedSeats1 = length (filter (== Occupied) (cells part1UpdatedGrid))
  putStrLn ("\nSeats occupied using part 1 rules: " ++ show occupiedSeats1)
  putStrLn "\n"
  putStrLn "Calculating seats occupied using fixed point of applying part 2 rules..."
  let part2UpdatedGrid = updateSeats part2Rules grid
      occupiedSeats2 = length (filter (== Occupied) (cells part2UpdatedGrid))
  putStrLn ("\nSeats occupied using part 2 rules: " ++ show occupiedSeats2)
