module Days.Day13  where

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

import Control.Monad.State

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Debug.Trace

import Data.Function ((&))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
  line = do
    x <- decimal
    string ": "
    y <- decimal
    return (x,y)
  in line `sepBy1` endOfLine

------------ TYPES ------------
type Input = [(Int,Int)]

data Frame = Frame {
  width :: Int,
  ranges :: Map Int Int,
  scanners :: Map Int (Direction,Int),
  pos :: Int,
  severity :: Int
}
  deriving (Show)

data Direction = Up | Down
  deriving (Show)

firstFrame :: Input -> Frame
firstFrame input = let
    width = maximum $ map fst input
    ranges = Map.fromList input
    scanners = Map.fromList $ map (\(x,_) -> (x,(Down, 0))) input
    pos = 0
    severity = 0
  in Frame{..}

getRange :: Int -> State Frame Int
getRange n = gets (Map.findWithDefault 0 n . ranges)

moveRight = modify (\f@Frame{..} -> f { pos = pos + 1 })

moveScanners = do
  f @ Frame{..} <- get
  let
    move key (dir, loc) =
      let
        loc' = case dir of
          Up -> loc - 1
          Down -> loc + 1
        dir' = let range = ranges Map.! key in if
          | loc' == range-1 -> Up
          | loc' == 0     -> Down
          | otherwise     -> dir
      in (dir', loc')
  put f { scanners = Map.mapWithKey move scanners }

recordSeverity = do
  f @ Frame{..} <- get
  let
    severity' = case (Map.lookup pos scanners) of
      Nothing    -> 0
      Just (_,s) -> if s == 0 then pos * (ranges Map.! pos) else 0
  put f { severity = severity + severity' }

step = do
  moveRight
  moveScanners
  recordSeverity

wait = do
  moveScanners
  recordSeverity

getSeverity initialFrame = initialFrame
  & evalState (sequence $ repeat $ step >> get)
  & find (\Frame{..} -> pos == width)
  & fromJust
  & severity


------------ PART A ------------
partA :: Input -> Int
partA input = getSeverity $ firstFrame input


------------ PART B ------------
partB :: Input -> Int
partB input = let
    fs = [ \x -> (x+d) `mod` (2*(r-1)) /= 0 | (d,r) <- input]
    f wait = and $ map ($ wait) fs
  in head $ filter f [0..]
