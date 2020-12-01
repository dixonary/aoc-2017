module Days.Day02 (runDay) where

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

import Data.Ratio 
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (decimal `sepBy1` (char '\t')) `sepBy` endOfLine

------------ TYPES ------------
type Input = [[Int]]

------------ PART A ------------
partA :: Input -> Int
partA input = sum $ map getMinMax input
    where
      getMinMax row = maximum row - minimum row

------------ PART B ------------
partB :: Input -> Int
partB input = sum $ map getNumerator input
    where
      getNumerator row = head 
            [ numerator r
            | x <- row
            , y <- row
            , let r = x % y
            , numerator r > 1
            , denominator r == 1
            ]
