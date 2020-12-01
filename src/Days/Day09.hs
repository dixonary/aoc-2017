module Days.Day09 (runDay) where

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

import Control.Applicative.Combinators hiding (choice, sepBy)

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void

import Data.Functor
import Data.Bifunctor (first)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
  group n = do
    char '{' 
    results <- (garbage <|> group (n+1)) `sepBy` char ','
    char '}'
    return (sum (fst <$> results) + n, sum (snd <$> results))
  garbage = do
    char '<'
    garbo <- many $ (char '!' >> anyChar $> 0) <|> (satisfy (/= '>') $> 1)
    char '>'
    return (0, sum garbo)
  in group 1

------------ TYPES ------------
type Input = (Int, Int)

------------ PART A ------------
partA :: Input -> Int
partA = fst

------------ PART B ------------
partB :: Input -> Int
partB = snd
