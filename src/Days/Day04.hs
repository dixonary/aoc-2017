module Days.Day04 (runDay) where

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

import Data.Functor
import Data.Function
import Control.Monad.State

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = signed decimal `sepBy1` endOfLine

------------ TYPES ------------
type Input = [Int]

data Tape    = Tape {tape :: Vector Int, headPos :: Int}
type STape a = State Tape a

------------ PART A ------------
--partA :: Input -> Int
partA input = sequence (repeat jump)
            & flip evalState (Tape (Vec.fromList input) 0)
            & zip [1..]
            & find (\(step,ix) -> ix < 0 || ix >= length input)
            & fromJust
            & fst

getAt :: Int -> STape Int
getAt pos = gets ((Vec.! pos) . tape)

setAt :: Int -> Int -> STape ()
setAt pos val = modify (\t -> t {tape = tape t Vec.// [(pos,val)]})

moveHead :: Int -> STape ()
moveHead h = modify (\t -> t { headPos = h })

jump :: STape Int
jump = do
  pos <- gets headPos
  v   <- getAt pos
  setAt pos (v+1)
  let pos' = v + pos
  moveHead pos'
  return pos'

------------ PART B ------------
partB input = sequence (repeat jump')
            & flip evalState (Tape (Vec.fromList input) 0)
            & zip [1..]
            & find (\(step,ix) -> ix < 0 || ix >= length input)
            & fromJust
            & fst

jump' :: STape Int
jump' = do
  pos <- gets headPos
  v   <- getAt pos
  setAt pos (if v >= 3 then v-1 else v+1)
  let pos' = v + pos
  moveHead pos'
  return pos'