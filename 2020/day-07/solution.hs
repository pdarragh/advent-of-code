{-# LANGUAGE RecordWildCards, MultiWayIf #-}

import Control.Monad.State hiding (get)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP (
  ReadP,
  char, choice, get, many1, manyTill, munch1, optional, satisfy, sepBy, skipSpaces, string)
import Text.Read (readPrec, readP_to_Prec)
import Data.Tree
import Data.Tuple (swap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

-- Performs a Maybe action within a monadic context. If the left-hand action
-- succeeds, its result is returned. Otherwise, the right-hand side is run.
(<??>) :: Monad m => m (Maybe a) -> m a -> m a
(<??>) mmx dx = do
  mx <- mmx
  case mx of
    Nothing -> dx
    Just x  -> return x

-- Given an initial state and a stateful action, runs that action and returns
-- its result, discarding the state resulting from the action's execution.
(*->) :: s -> State s a -> a
(*->) s0 m = fst (s0 **> m)
infix 0 *->

-- Given an initial state and a stateful action, runs that action and returns
-- the action's result paired with the state after the action's execution.
(**>) :: s -> State s a -> (a, s)
(**>) = flip runState
infix 0 **>

-- Given a map containing Eithers, produces a pair of maps containing just the
-- Left and Right elements, respectively.
partitionOnEither :: IntMap.IntMap (Either a b) -> (IntMap.IntMap a, IntMap.IntMap b)
partitionOnEither = IntMap.foldrWithKey partitionOnEither' (IntMap.empty, IntMap.empty)
  where
    partitionOnEither' :: IntMap.Key -> Either a b -> (IntMap.IntMap a, IntMap.IntMap b) -> (IntMap.IntMap a, IntMap.IntMap b)
    partitionOnEither' k (Left  v) (am, bm) = (IntMap.insert k v am, bm)
    partitionOnEither' k (Right v) (am, bm) = (am, IntMap.insert k v bm)

-- These are semantics type aliases for convenience.
type Color = String
type ColorID = Int
type ColorToIDMap = Map.Map Color ColorID
type IDToColorMap = IntMap.IntMap Color
type ColorIDMap = IntMap.IntMap
type ColorIDSet = IntSet.IntSet
type RuleMap = ColorIDMap (ColorIDMap Int)
type RuleTree = Tree (ColorID, Int)

-- A representation of the internal state, where colors are mapped to IDs.
data ColorState = ColorState { colorsToIDs :: ColorToIDMap
                             , idsToColors :: IDToColorMap
                             , nextID      :: ColorID }
                deriving (Show)

-- The initial state, consisting of empty maps and the initial color ID.
initialColorState :: ColorState
initialColorState = ColorState { colorsToIDs = Map.empty
                               , idsToColors = IntMap.empty
                               , nextID      = 0 }

-- Adds a color to the state.
addColor' :: Color -> ColorState -> ColorState
addColor' color ColorState{..} = ColorState { colorsToIDs = Map.insert color nextID colorsToIDs
                                            , idsToColors = IntMap.insert nextID color idsToColors
                                            , nextID      = nextID + 1 }

-- Adds a color to the state, returning the new ID.
addColor :: Color -> State ColorState Int
addColor color = gets nextID <* modify (addColor' color)

-- Attempts to look up a color's ID in the state.
lookupColor :: Color -> State ColorState (Maybe ColorID)
lookupColor color = Map.lookup color <$> gets colorsToIDs

-- Converts a color's name to its ID, producing an error if the color is not
-- registered in the state.
colorToID :: Color -> State ColorState Int
colorToID color = fromJust <$> lookupColor color

-- Converts a color's name to its ID, registering the color in the state if it
-- has not been added before.
colorToIDOrAdd :: Color -> State ColorState Int
colorToIDOrAdd color = lookupColor color <??> addColor color

-- Converts a color's ID to its name.
idToColor :: ColorID -> State ColorState Color
idToColor colorID = fromJust . IntMap.lookup colorID <$> gets idsToColors

-- An initial state from which to work.
initialized :: ColorState
initialized = execState (addColor targetColor) initialColorState

-- Correlates a color to a list of colors that it can contain and the
-- containable quantity of each.
data RawContainmentRule = RawContainmentRule Color [(Int, Color)] deriving (Eq, Show)

-- Constructs a parser for raw containment rules.
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
      c <- manyTill (satisfy (\c -> c /= ',' && c /= '.')) (skipSpaces >> string "bag" >> optional (char 's'))
      return (read n, c)

instance Read RawContainmentRule where
  readPrec = readP_to_Prec (const readRawContainmentRule)

-- Correlates a color's ID to a list of colors that it can contain and the
-- containable quantity of each.
data ContainmentRule = ContainmentRule ColorID [(Int, ColorID)] deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
--
-- NOTE: The Part 1 code is more complicated than it needs to be. I was
--       specifically looking to do this as a fixed-point computation for fun.
--       Part 2 is more reasonable.
--
--------------------------------------------------------------------------------

-- Produces a list of color IDs that can contain the indicated color, based on
-- the rules provided in a given RuleMap.
colorIDsContainingColorID :: ColorID -> RuleMap -> [ColorID]
colorIDsContainingColorID targetID rules = fixContainers (partitionOnEither simplifiedMap)
  where
    -- We start by simplifying the initial rules. This gives us a better initial
    -- state than we would otherwise have.
    simplifiedMap :: ColorIDMap (Either Bool ColorIDSet)
    simplifiedMap = IntMap.map simplifyRule rules
    -- Simplifies a rule, merely looking at whether the indicated color either
    -- can contain the target color or can't contain any colors at all.
    simplifyRule :: ColorIDMap Int -> Either Bool ColorIDSet
    simplifyRule cids = if
      | IntMap.null cids            -> Left False
      | IntMap.member targetID cids -> Left True
      | otherwise                   -> Right (IntMap.keysSet cids)
    -- Performs a fixed point reduction to identify which color IDs can contain
    -- the target color ID.
    fixContainers :: (ColorIDMap Bool, ColorIDMap ColorIDSet) -> [ColorID]
    fixContainers (resolvedContainers, unresolvedContainers)
      | IntMap.null unresolvedContainers
        -- There are no containers left to reduce, so our Bools contain the
        -- answers!
          = map fst (IntMap.toList (IntMap.filter id resolvedContainers))
      | resolvedContainers == newResolvedContainers &&
        unresolvedContainers == newUnresolvedContainers
        -- One of the maps is identical to the previous iteration's. This means
        -- we will no longer make progress, but that we also have not finished
        -- reducing everything. (This is bad.)
          = error "fixed-point reached without making sufficient progress"
      | otherwise
        -- There remain some containers to reduce. We reduce what we can and
        -- merge the result into what we already have.
          = fixContainers (newResolvedContainers, newUnresolvedContainers)
      where
        -- We merge the previously resolved containers with the containers that
        -- we have just resolved, favoring the mappings of the latter (to
        -- preserve progress).
        newResolvedContainers :: ColorIDMap Bool
        newResolvedContainers = IntMap.union (fst reducedContainerMaps) resolvedContainers
        -- The now-remaining unresolved containers are whatever is left.
        newUnresolvedContainers :: ColorIDMap ColorIDSet
        newUnresolvedContainers = snd reducedContainerMaps
        -- Reduces each of the yet-unresolved container IDs, then partitions the
        -- result.
        reducedContainerMaps :: (ColorIDMap Bool, ColorIDMap ColorIDSet)
        reducedContainerMaps = partitionOnEither (IntMap.map reduceRule unresolvedContainers)
        -- Folds a reduction over the set of IDs yet to be resolved. If the
        -- first element of the tuple comes back as True, we've found at least
        -- one sub-container that can contain the target ID, which means that
        -- the current container can also contain the target ID. Otherwise, we
        -- have a (hopefully smaller) set of sub-container IDs to check next
        -- time, once progress has been made elsewhere.
        reduceRule :: ColorIDSet -> Either Bool ColorIDSet
        reduceRule cids = case IntSet.foldr singleStepReduce (False, IntSet.empty) cids of
            (True, _)     -> Left True
            (_,    cids') -> if
              | IntSet.null cids' -> Left False
              | otherwise         -> Right cids'
        -- Performs a single-step reduction. Here, we do a lookup on each
        -- sub-container ID to see whether it has been reduced. If it has, we
        -- accumulate the result (via (||)). If it hasn't, we leave it for
        -- later.
        singleStepReduce :: IntSet.Key -> (Bool, ColorIDSet) -> (Bool, ColorIDSet)
        singleStepReduce k (b, cs) = case IntMap.lookup k resolvedContainers of
            Nothing   -> (b, IntSet.insert k cs)
            Just bool -> (bool || b, cs)

-- Converts a list of containment rules into a map of rules. This is easier to
-- process quickly.
mapifyRules :: [ContainmentRule] -> RuleMap
mapifyRules = IntMap.fromList . map flattenRule
  where
    flattenRule :: ContainmentRule -> (ColorID, ColorIDMap Int)
    flattenRule (ContainmentRule colorID rules) = (colorID, IntMap.fromList (map swap rules))

-- Converts a raw rule (using strings as color names) to a containment rule
-- (which uses IDs instead of strings) within our state.
convertRule :: RawContainmentRule -> State ColorState ContainmentRule
convertRule (RawContainmentRule color rules) = do
  colorID <- colorToIDOrAdd color
  let (quantities, colors) = unzip rules
  colorIDs <- mapM colorToIDOrAdd colors
  return (ContainmentRule colorID (zip quantities colorIDs))

-- Finds a list of colors (as strings) that can contain the indicated color.
colorsContainingColorInRules :: [RawContainmentRule] -> Color -> State ColorState [Color]
colorsContainingColorInRules rawRules targetColorName = do
  ruleMap <- mapM convertRule rawRules <&> mapifyRules
  targetID <- colorToID targetColorName
  let colorIDs = colorIDsContainingColorID targetID ruleMap
  mapM idToColor colorIDs

--------------------------------------------------------------------------------
--
-- This is the code for Part 2.
--
--------------------------------------------------------------------------------

-- Combines the elements of a tree, using a different function for the child-
-- to-parent combinations and the child-to-child combinations. Both directions
-- of combination take the parent (start) node's value as input first.
combineTree :: (a -> b -> b) -> (a -> [b] -> b) -> Tree a -> b
combineTree combineDown combineAcross tree =
  combineAcross v (map (combineDown v . combineTree combineDown combineAcross) children)
  where
    v = rootLabel tree
    children = subForest tree

-- Sums the quantities of a color tree.
sumChildrenInclusive :: RuleTree -> Int
sumChildrenInclusive tree = combineTree (*) (foldr (+)) quantityTree - rootQuantity
  where
    quantityTree = fmap snd tree
    rootQuantity = rootLabel quantityTree

-- Constructs a tree from the mapping of rules.
treeFromRulesWithRoot :: RuleMap -> ColorID -> RuleTree
treeFromRulesWithRoot rules rootID = unfoldTree treeFromRule (rootID, 1)
  where
    treeFromRule :: (ColorID, Int) -> ((ColorID, Int), [(ColorID, Int)])
    treeFromRule (colorID, quantity) = ((colorID, quantity), IntMap.toList (rules IntMap.! colorID))

-- Produces a tree rooted at the given color.
treeFromColor :: [RawContainmentRule] -> Color -> State ColorState RuleTree
treeFromColor rawRules targetColorName = do
  ruleMap <- mapM convertRule rawRules <&> mapifyRules
  targetID <- colorToID targetColorName
  return (treeFromRulesWithRoot ruleMap targetID)

-- Converts an input file into a list of raw containment rules.
readInputFile :: String -> IO [RawContainmentRule]
readInputFile fileName = do
  content <- readFile fileName
  return (map read (lines content))

-- The name of the source file to read inputs from.
sourceFile :: String
sourceFile = "input.txt"

-- This is the special, target color we are working with, per the instructions.
targetColor :: Color
targetColor = "shiny gold"

main :: IO ()
main = do
  rawRules <- readInputFile sourceFile
  let colors = initialized *-> colorsContainingColorInRules rawRules targetColor
  putStrLn ("There are " ++ show (length colors) ++ " colors of bags that can contain a " ++ targetColor ++ " bag.")
  let numberOfBags = initialized *-> treeFromColor rawRules targetColor <&> sumChildrenInclusive
  putStrLn ("The " ++ targetColor ++ " bag contains " ++ show numberOfBags ++ " other bags.")
