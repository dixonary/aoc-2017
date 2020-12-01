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
import Data.Attoparsec.Text
import Data.Void

import Util.Util
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 letter `sepBy1` (char ' ')) `sepBy` endOfLine

------------ TYPES ------------
type Input = [[String]]

------------ PART A ------------
partA :: Input -> Int
partA = length . filter (\ls -> let s = sort ls in nub s == s)

------------ PART B ------------
partB :: Input -> Int
partB = length . filter (\ls -> let s = sort ls in nub s == s) . map (map freq)
