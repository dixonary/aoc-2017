module Days.Day06 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void

import qualified Data.Vector.Mutable as MVec

import Data.Function

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy1` char '\t'

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
partA input = secondIndex
  where
    ls = iterate step (Vec.fromList input)
    U.Repeat{..} = fromJust $ U.firstRepeat ls

step :: Vector Int -> Vector Int
step vec = do
  let 
    m  = Vec.maximum  vec
    ix = Vec.maxIndex vec
  Vec.accum (+) -- Accumulates all sums onto their respective indices
    (Vec.modify (\v -> MVec.write v ix 0) vec) -- Zero out the distributed value
    (map (,1) $ take m $ drop (ix+1) $ cycle [0..length vec-1]) -- Distribute

------------ PART B ------------
partB :: Input -> Int
partB input = secondIndex - firstIndex
  where
    ls = iterate step (Vec.fromList input)
    U.Repeat{..} = fromJust $ U.firstRepeat ls
