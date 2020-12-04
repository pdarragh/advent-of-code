import Text.ParserCombinators.ReadP ((+++), ReadP, char, eof, gather, get, manyTill, optional, pfail, sepBy, skipMany1)
import Text.Read (readPrec, readP_to_Prec)

import Debug.Trace (trace)

sourceFile :: String
sourceFile = "input.txt"

data Cell = Open
          | Tree

instance Show Cell where
  show Open = "."
  show Tree = "#"

readCell :: ReadP Cell
readCell = do
  c <- char '.' +++ char '#'
  case c of
    '.' -> return Open
    '#' -> return Tree
    _   -> pfail

instance Read Cell where
  readPrec = readP_to_Prec (const readCell)

newtype Row = Row [Cell]

instance Show Row where
  show (Row cells) = concatMap show cells

readRow :: ReadP Row
readRow = do
  cells <- manyTill readCell eof
  return (Row cells)

instance Read Row where
  readPrec = readP_to_Prec (const readRow)

newtype Board = Board [Row]

instance Show Board where
  show (Board rows) = unlines (map show rows)

readBoardFromFile :: String -> IO Board
readBoardFromFile fileName = do
  content <- readFile fileName
  return (Board (map read (lines content)))

main :: IO ()
main = do
  board <- readBoardFromFile sourceFile
  print board
