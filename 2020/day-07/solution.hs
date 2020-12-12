{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State hiding (get)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP (
  ReadP,
  char, choice, get, many1, manyTill, munch1, optional, satisfy, sepBy, skipSpaces, string)
import Text.Read (readPrec, readP_to_Prec)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

sourceFile :: String
sourceFile = "input.txt"

(<??>) :: Monad m => m (Maybe a) -> m a -> m a
(<??>) mmx dx = do
  mx <- mmx
  case mx of
    Nothing -> dx
    Just x  -> return x

(*->) :: s -> State s a -> a
(*->) s0 m = fst (s0 **> m)
infix 0 *->

(**>) :: s -> State s a -> (a, s)
(**>) = flip runState
infix 0 **>

type Color = String
type ColorID = Int
type ColorToIDMap = Map.Map Color ColorID
type IDToColorMap = IntMap.IntMap Color

data MapState = MapState { colorsToIDs :: ColorToIDMap
                         , idsToColors :: IDToColorMap
                         , nextID      :: ColorID }
                deriving (Show)

initialMapState :: MapState
initialMapState = MapState { colorsToIDs = Map.empty
                           , idsToColors = IntMap.empty
                           , nextID      = 0 }

addColor' :: Color -> MapState -> MapState
addColor' color MapState{..} = MapState { colorsToIDs = Map.insert color nextID colorsToIDs
                                        , idsToColors = IntMap.insert nextID color idsToColors
                                        , nextID      = nextID + 1 }

addColor :: Color -> State MapState Int
addColor color = gets nextID <* modify (addColor' color)

lookupColor :: Color -> State MapState (Maybe ColorID)
lookupColor color = Map.lookup color <$> gets colorsToIDs

colorToIDOrAdd :: Color -> State MapState Int
colorToIDOrAdd color = lookupColor color <??> addColor color

idToColor :: ColorID -> State MapState Color
idToColor colorID = fromJust . IntMap.lookup colorID <$> gets idsToColors

shinyGold :: Color
shinyGold = "shiny gold"

initialized :: MapState
initialized = execState (addColor shinyGold) initialMapState

data RawContainmentRule = RawContainmentRule Color [(Int, Color)] deriving (Eq, Show)

readRawContainmentRule :: ReadP RawContainmentRule
readRawContainmentRule = do
  color <- many1 get
  _     <- string " bags contain "
  rules <- choice [string "no other bags" >> return [], readBags]
  _     <- char '.'
  return (RawContainmentRule color rules)
  where
    readBags :: ReadP [(Int, Color)]
    readBags = sepBy readBag (char ',' >> skipSpaces)
    readBag :: ReadP (Int, Color)
    readBag = do
      n <- munch1 isDigit
      _ <- skipSpaces
      c <- manyTill (satisfy (\c -> c /= ',' && c /= '.')) (string "bag")
      _ <- optional (char 's')
      return (read n, c)

instance Read RawContainmentRule where
  readPrec = readP_to_Prec (const readRawContainmentRule)

data ContainmentRule = ContainmentRule ColorID [(Int, ColorID)] deriving (Show)

convertRule :: RawContainmentRule -> State MapState ContainmentRule
convertRule (RawContainmentRule color rules) = do
  colorID <- colorToIDOrAdd color
  let (quantities, colors) = unzip rules
  colorIDs <- mapM colorToIDOrAdd colors
  return (ContainmentRule colorID (zip quantities colorIDs))

readInputFile :: String -> IO [RawContainmentRule]
readInputFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

main :: IO ()
main = do
  rawRules <- readInputFile sourceFile
  let rules = initialized *-> mapM convertRule rawRules
  mapM_ print (zip [1..] rules)
