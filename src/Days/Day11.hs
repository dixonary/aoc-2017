module Days.Day11 (runDay) where

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
import Data.Functor (($>))
import Data.Ratio
{- ORMOLU_ENABLE -}

import Data.Ord (comparing)
import Control.Arrow ((>>>))

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  let dir = choice $ map (\(x,y)->(string x $> y))
            [ ("nw", NW)
            , ("ne", NE)
            , ("n" , N )
            , ("sw", SW)
            , ("se", SE)
            , ("s" , S )
            ]
  in dir `sepBy` char ','

------------ TYPES ------------
type Input = [HexDir]

data HexDir      = N
            | NW     | NE
            | SW     | SE
                 | S
  deriving (Show)

type Coord = (Ratio Int,Ratio Int)
(x,y) .+. (x',y') = (x+x', y+y')

toEuclid :: HexDir -> Coord
toEuclid N  = ( 0,-1)
toEuclid S  = ( 0, 1)
toEuclid NW = (-1%2,-1%2)
toEuclid NE = (1%2, -1%2)
toEuclid SW = (-1%2,1%2)
toEuclid SE = (1%2, 1%2)

hexDistance :: Coord -> Coord -> Ratio Int
hexDistance (x,y) (x',y') =
    let
      dx = x' - x
      dy = y' - y
      dz = 0 - dx - dy
    in (abs dx + abs dy + abs dz) * (1 % 2)

------------ PART A ------------
partA :: Input -> Ratio Int
partA = fmap toEuclid
      >>> foldr1 (.+.)
      >>> hexDistance (0,0)

------------ PART B ------------
partB :: Input -> Ratio Int
partB = fmap toEuclid
      >>> scanl1 (.+.)
      >>> maximumBy (comparing (hexDistance (0,0)))
      >>> hexDistance (0,0)
