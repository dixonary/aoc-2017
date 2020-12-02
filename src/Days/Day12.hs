module Days.Day12 (runDay) where

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

import Data.Graph

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void

import qualified Data.Foldable as Foldable

import Data.Function ((&))
import Data.Bifunctor (second)
import Debug.Trace

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
        line = do
            x <- decimal
            string " <-> "
            ys <- decimal `sepBy1` string ", "
            return (x,ys)
    in line `sepBy` endOfLine

------------ TYPES ------------
type Input = [(Int, [Int])]

sccs :: Input -> [[Int]]
sccs input = input
           & fmap (\(x,y) -> (x,x,y))
           & graphFromEdges
           & (\(a,b,c) -> a)
           & components
           & fmap Foldable.toList

------------ PART A ------------
partA :: Input -> Int
partA = length . fromJust . find (elem 0) . sccs


------------ PART B ------------
partB :: Input -> Int
partB = length . sccs
