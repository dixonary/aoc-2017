{-# LANGUAGE BinaryLiterals #-}
module Days.Day15 (runDay) where

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

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Bits
import Data.Function (on)
import Text.Printf
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "Generator A starts with "
  a <- decimal
  skipSpace
  string "Generator B starts with "
  b <- decimal
  return (a,b)

------------ TYPES ------------
type Input = (Int,Int)

imax = 2147483647

-- This solution gives the right answers for
-- a,b on the example data
-- a on my own data
-- NOT b on my own data :(

observe propA propB iters (seedA,seedB) = let
  mul n x = (x * n) `mod` imax
  as = filter propA $ iterate (mul 16807) seedA
  bs = filter propB $ iterate (mul 48271) seedB
  sameLowest = (==) `on` (`mod` 2^16)
  in foldl' (+) 0
      $ map fromEnum
      $ Prelude.take iters
      $ zipWith sameLowest as bs

------------ PART A ------------
partA :: Input -> Int
partA = observe
        (const True)
        (const True)
        40_000_000

------------ PART B ------------
partB :: Input -> Int
partB = observe
        (\x -> (x `mod` 4) == 0)
        (\x -> (x `mod` 8) == 0)
        5_000_000
