module Days.Day01 (runDay) where

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

import Control.Applicative.Combinators

import Data.Functor

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many $ digit <&> (read.pure)


------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
partA input = sum $ zipWith f input (tail $ cycle input)
    where 
      f x y = if x == y then x else 0

------------ PART B ------------
partB :: Input -> Int
partB input = sum $ zipWith f input (drop (length input `div` 2) $ cycle input)
    where
      f x y = if x == y then x else 0