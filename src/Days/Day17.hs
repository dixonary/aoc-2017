module Days.Day17 (runDay) where

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

import Data.List.PointedList.Circular
import qualified Data.List.PointedList.Circular as CTape

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Debug.Trace

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal

------------ TYPES ------------
type Input = Int

------------ PART A ------------
partA :: Input -> Int
partA dist = let
    initialList = fromJust $ CTape.fromList [0]
    step tape n = insertRight n $ moveN dist tape
    in _focus $ next $ foldl' step initialList [1..2017]

------------ PART B ------------
partB :: Input -> Int
partB dist = runST $ do
    pos <- newSTRef 0
    len <- newSTRef 1
    valueAfterZero <- newSTRef 0
    forM_ [1..50_000_000] $ \v -> do
      l <- readSTRef len
      p <- readSTRef pos
      let pos' = (p + dist) `mod` l
      writeSTRef pos (pos'+1)
      writeSTRef len (l+1)
      when (pos' == 0) $ writeSTRef valueAfterZero v
    readSTRef valueAfterZero

