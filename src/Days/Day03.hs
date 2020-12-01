module Days.Day03 (runDay) where

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
import Data.Function
import Control.Monad.State
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
partA n = let (x,y) = locateVal n in abs x + abs y
  

locateVal :: Int -> Coord
locateVal n = migrate (ix,ix) (rank*rank - n)
  where
    Just (ix, rank) = map (\x -> (x,x*2+1)) [0..]
              & find (\(ix,rank) -> rank*rank >= n) 

    -- move the point around by the requisite amount
    migrate (x,y) dist = 
      let
        maxdist = rank - 1
        move (dx,dy) d (x,y) = 
          let d' = max 0 $ min d maxdist
          in (x + dx * d',y + dy * d')
      in (x,y) 
        & move (-1,0) (dist)              -- left 
        & move (0,-1) (dist - maxdist)    -- up
        & move (1,0)  (dist - maxdist*2)  -- right
        & move (0,1)  (dist - maxdist*3)  -- down



------------ PART B ------------
partB :: Input -> Int
partB n = sequence (add <$> [2..])
        & flip evalState (Map.fromList [((0,0),1)])
        & find (> n)
        & fromJust

-- Stateful bois
type Coord = (Int,Int)
type Grid = Map Coord Int
type Day3 a = State Grid a

val :: Coord -> Day3 (Maybe Int)
val (x,y) = gets (Map.lookup (x,y))

sumNearby :: Coord -> Day3 Int
sumNearby (x,y) = do
  vals <- sequence 
      [ val (x',y') 
      | x' <- [x-1..x+1]
      , y' <- [y-1..y+1]
      , x' /= x || y' /= y
      ]
  return $ sum $ fromMaybe 0 <$> vals

add :: Int -> Day3 Int
add n = do
  let pos = locateVal n
  val <- sumNearby pos
  modify (Map.insert pos val)
  return val



