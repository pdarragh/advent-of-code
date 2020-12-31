{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP
import Text.Read (readListPrec, readPrec, readP_to_Prec)

data CellState
  = Floor
  | Empty
  | Occupied
  deriving (Eq)

instance Show CellState where
  show Floor = "."
  show Empty = "L"
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

data Coordinate = Coordinate { getX :: Int
                             , getY :: Int }
                  deriving (Eq)

instance Show Coordinate where
  show Coordinate{..} = "(" ++ show getX ++ ", " ++ show getY ++ ")"

data Seat = Seat CellState Coordinate deriving (Eq, Show)

readInputFile :: String -> IO [[CellState]]
readInputFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  cells <- readInputFile sourceFile
  mapM_ print cells
