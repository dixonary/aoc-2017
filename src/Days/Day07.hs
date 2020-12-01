module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import Data.Tree (Tree)
import qualified Data.Tree as Tree

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = 
  let
    line = do
      x <- many1 letter
      char ' '
      n <- char '(' *> decimal <* char ')'
      children <- option [] $ string " -> " >> many1 letter `sepBy1` string ", "
      return (x,(n,children))
  in line `sepBy` endOfLine
    

------------ TYPES ------------
type Input = [(String, (Int, [String]))]

------------ PART A ------------
partA :: Input -> String
partA input = let 
    -- Construct the parent relation inside of a map based on a line 
    f :: Map String String -> (String, (Int, [String])) -> Map String String
    f m (name, (val, children)) = foldr (flip Map.insert name) m children

    parents = foldl' f Map.empty input
    allNames = fst <$> input
    
  in fromJust $ find (not . (`Map.member` parents)) allNames

------------ PART B ------------
partB :: Input -> Int
partB input = let
    -- Build a "proper" tree using the root found in part A
    inputMap = Map.fromList input
    tree = Tree.unfoldTree (inputMap Map.!) (partA input)

    -- Recursively search for the erroneous boi
    makePaths :: Int -> [Either Int Node] -> Either Int Node
    makePaths x children = do
        cws <- sequence children
        
        let subs = fmap totalWeight cws
            wfreq = fmap fst $ sortOn snd $ Map.toList $ U.freq $ subs 
            mode   = last wfreq
            unmode = head wfreq
        if 
          | null cws        -> return $ Node True x x
          | mode == unmode  -> return $ Node False (x + sum subs) x
          | otherwise       -> 
              let 
                diff = mode - unmode 
                Just wrongVal = find (\Node{..} -> totalWeight == unmode) cws
              in 
                Left $ ownWeight wrongVal + diff
              
  in (\(Left res) -> res) $ Tree.foldTree makePaths tree

data Node = Node {
  isLeaf :: Bool,
  totalWeight :: Int,
  ownWeight :: Int
}