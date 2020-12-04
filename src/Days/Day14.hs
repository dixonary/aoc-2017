{-# LANGUAGE TypeApplications #-}
module Days.Day14 (runDay) where

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

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative.Combinators

import qualified Data.Foldable as Foldable
import Util.Parsers as P

import qualified Data.Text as T
import Text.Printf

import Data.Either (fromRight)

import Days.Day10 (hash)
import Data.Function ((&))
import Debug.Trace

import Data.Graph (Graph)
import qualified Data.Graph as Graph

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many (notChar '\n')

------------ TYPES ------------
type Input = String
type Coord = (Int,Int)

------------ PART A ------------
partA :: Input -> Int
partA = sum . fmap (length . getOnes) . makeBoard

-- Get those places of a row which are ones
getOnes = map fst . filter snd . zip [0..127]

-- Construct the binary string rows
makeBoard input = [ makeRow $ concat [input,"-",show i]
                  | i <- [0..127]
                  ]

-- translate a hash into a bit list
makeRow :: String -> [Bool]
makeRow x = hash x
  & parseOnly (hexadecimal @Integer) . T.pack
  & fromRight 0
  & printf "%0128b"
  & map (=='1')

------------ PART B ------------
partB :: Input -> Int
partB input = let
    coords = concat
      [ (,y) <$> getOnes row
      | (row,y) <- zip (makeBoard input) [0..]
      ]
  in numRegions coords

numRegions :: [Coord] -> Int
numRegions coords =
  let
    edgeList =
      [ (i,coord,edges)
      | (i,coord@(x,y)) <- zip [0..] coords
        -- Observe we only consider (down,right) since
        -- `components` is bidirectional
      , let edges = [(x+1,y),(x,y+1)] `intersect` coords
      ]
    (g,_,_) = Graph.graphFromEdges edgeList
  in length $ Graph.components g
