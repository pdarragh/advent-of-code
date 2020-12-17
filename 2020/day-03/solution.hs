import Text.ParserCombinators.ReadP ((+++), ReadP, char, endBy, many, pfail)
import Text.Read (readPrec, readP_to_Prec)

-- A description of squares that either have a tree... or don't.
data Cell = Open
          | Tree

instance Show Cell where
  -- Renders a cell the same way they come in.
  show Open = "."
  show Tree = "#"

-- Defines a parser for reading a cell.
readCell :: ReadP Cell
readCell = do
  c <- char '.' +++ char '#'
  case c of
    '.' -> return Open
    '#' -> return Tree
    _   -> pfail

instance Read Cell where
  -- Reads a cell from a string.
  readPrec = readP_to_Prec (const readCell)

-- Cells are arranged in rows!
newtype Row = Row [Cell]

-- Like cells, rows can also be shown.
instance Show Row where
  show (Row cells) = concatMap show cells

-- Defines a parser for reading a row.
readRow :: ReadP Row
readRow = do
  cells <- many readCell
  return (Row cells)

instance Read Row where
  -- Reads a row from a string.
  readPrec = readP_to_Prec (const readRow)

-- Boards are arranged in rows of rows! Gasp!
newtype Board = Board [Row]

instance Show Board where
  -- Renders a board the same way they are specified.
  show (Board rows) = unlines (map show rows)

-- Defines a parser for reading a board.
readBoard :: ReadP Board
readBoard = do
  rows <- endBy readRow (char '\n')
  return (Board rows)

instance Read Board where
  -- Reads a board from a string.
  readPrec = readP_to_Prec (const readBoard)

-- Access an (x, y) coordinate on the board, starting from the top-left corner.
access :: Board -> Int -> Int -> Cell
access (Board rows) x y = row !! mod x (length row) where
  Row row = rows !! y

-- These are the paths defined in the instructions.
paths :: [(Int, Int)]
paths =
  [ (1, 1)
  , (3, 1)
  , (5, 1)
  , (7, 1)
  , (1, 2)
  ]

-- This is the default path, used in part 1.
defaultPath :: (Int, Int)
defaultPath = paths !! 1

-- Count the trees that the toboggan would hit starting at the top-left corner
-- and constantly moving right by `dx` and down by `dy` until the bottom of the
-- board is reached.
--
-- NOTE: Although the rows are specified with finite length, they are actually
--       assumed to repeat indefinitely, hence the modulo indexing.
countTrees :: Board -> (Int, Int) -> Int
countTrees board@(Board rows) (dx, dy) = countTrees' 0 0 0 where
  countTrees' :: Int -> Int -> Int -> Int
  countTrees' trees x y
    | y >= length rows = trees
    | otherwise = countTrees' (trees + newTree) (x + dx) (y + dy) where
        newTree = case access board x y of
          Open -> 0
          Tree -> 1

-- Count the trees that would be hit by following the default path.
countTreesInDefaultPath :: Board -> Int
countTreesInDefaultPath board = countTrees board defaultPath

-- Converts a file whose contents are a specification of a map into that map
-- (which we call a `Board` here).
readBoardFromFile :: String -> IO Board
readBoardFromFile fileName = do
  content <- readFile fileName
  return (read content)

sourceFile :: String
sourceFile = "input.txt"

main :: IO ()
main = do
  board <- readBoardFromFile sourceFile
  let trees = countTreesInDefaultPath board
  putStrLn "Default Path:"
  putStrLn ("  There are " ++ show trees ++ " trees in the 3/1 path beginning at the top left of the board.")
  let counts = map (countTrees board) paths
  putStrLn "All Paths:"
  mapM_ (putStrLn . (\((dx, dy), count) ->
                       "  There are " ++ show count ++ " trees in the "
                       ++ show dx ++ "/" ++ show dy
                       ++ " path beginning at the top left of the board."))
    (zip paths counts)
  putStrLn ("The product of these counts is: " ++ show (product counts))
