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
import Data.Maybe
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
type Input = (Integer,Integer)

imax = 2147483647

observe iters propA propB (seedA,seedB) = let
  mul n x = (x * n) `mod` imax
  as = filter propA $ tail $ iterate (mul 16807) seedA
  bs = filter propB $ tail $ iterate (mul 48271) seedB
  a `matches` b = (a `mod` 2^16) == (b `mod` 2^16)
  in length
      $ filter id
      $ Prelude.take iters
      $ zipWith matches as bs

------------ PART A ------------
partA :: Input -> Int
partA = observe
        40_000_000
        (const True) -- take every number
        (const True) -- take every number

------------ PART B ------------
partB :: Input -> Int
partB = observe
        5_000_000
        (\x -> (x `mod` 4) == 0) -- only take mults of 4
        (\x -> (x `mod` 8) == 0) -- only take mults of 8

