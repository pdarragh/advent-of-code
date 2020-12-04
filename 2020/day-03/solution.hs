import Text.ParserCombinators.ReadP ((+++), ReadP, char, eof, manyTill, pfail)
import Text.Read (readPrec, readP_to_Prec)

sourceFile :: String
sourceFile = "input.txt"

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
  cells <- manyTill readCell eof
  return (Row cells)

instance Read Row where
  -- Reads a row from a string.
  readPrec = readP_to_Prec (const readRow)

-- Boards are arranged in rows of rows! Gasp!
newtype Board = Board [Row]

instance Show Board where
  -- Renders a board the same way they are specified.
  show (Board rows) = unlines (map show rows)

-- NOTE: `Board` didn't get a `Read` instance because it wasn't working and I
--       decided to just move on with things. :shrug:

-- Access an (x, y) coordinate on the board, starting from the top-left corner.
access :: Board -> Int -> Int -> Cell
access (Board rows) x y = row !! mod x (length row) where
  Row row = rows !! y

-- Count the trees that the toboggan would hit starting at the top-left corner
-- and constantly moving right 3 and down 1 until the bottom of the board is
-- reached.
--
-- NOTE: Although the rows are specified with finite length, they are actually
--       assumed to repeat indefinitely, hence the modulo indexing.
countTreesInDefaultPath :: Board -> Int
countTreesInDefaultPath board@(Board rows) = countTrees 0 0 3 1 0 where
  countTrees :: Int -> Int -> Int -> Int -> Int -> Int
  countTrees x y dx dy trees
    | y >= length rows = trees
    | otherwise = countTrees (x + dx) (y + dy) dx dy (trees + newTree) where
        newTree = case access board x y of
          Open -> 0
          Tree -> 1

-- Converts a file whose contents are a specification of a map into that map
-- (which we call a `Board` here).
readBoardFromFile :: String -> IO Board
readBoardFromFile fileName = do
  content <- readFile fileName
  return (Board (map read (lines content)))

main :: IO ()
main = do
  board <- readBoardFromFile sourceFile
  let trees = countTreesInDefaultPath board
  putStrLn ("There are " ++ show trees ++ " trees in the 3/1 path beginning at the top left of the board.")
