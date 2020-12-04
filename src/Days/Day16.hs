{-# LANGUAGE BangPatterns #-}
module Days.Day16 (runDay) where

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

import Control.Monad.ST
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MVec
import Control.Monad
import Debug.Trace
import Data.Char (ord)
import Data.Either
import Data.Tuple (swap)
import Data.Semigroup
import Control.Arrow ((>>>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    danceMove = choice
      [ char 's' >> fmap Spin decimal
      , char 'x' >> (Exchange <$> decimal <*> (char '/' >> decimal))
      , char 'p' >> (Partner <$> anyChar <*> (char '/' >> anyChar))
      ]
  in danceMove `sepBy` (char ',')

------------ TYPES ------------
type Input = [DanceMove]

type DanceLine s = STVector s Dancer
type Dancer    = Char

data DanceMove
  = Spin Int
  | Exchange Int Int
  | Partner Dancer Dancer
  deriving (Show)

-- A permutation maps initial position to final position.
newtype Perm = Perm (Map Int Int)
instance Semigroup Perm where
  (Perm !ma) <> (Perm !mb) = Perm (U.composeMaps ma mb)
instance Monoid Perm where
  mempty = Perm refl

-- A rename modifies a permutation to switch
-- positions around.
newtype Rename = Rename (Perm -> Perm)
instance Semigroup Rename where
  (Rename !a) <> (Rename !b) = Rename (a >>> b)
instance Monoid Rename where
  mempty = Rename id

range = [0..15]
total = 16

refl :: Map Int Int
refl = Map.fromList $ zip range range

-- Function which converts chars to vals
ctoi = ((Map.fromList $ zip ['a'..'p'] [0..]) Map.!)
itoc = ((Map.fromList $ zip [0..] ['a'..'p']) Map.!)

perm :: DanceMove -> Perm
perm (Spin n)       = Perm $ Map.fromList [((x+n) `mod` total,x) | x <- range]
perm (Exchange i j) = Perm $ foldr (uncurry Map.insert) refl [(i,j),(j,i)]
perm (Partner a b)  = mempty

rename :: DanceMove -> Rename
rename (Spin n)       = mempty
rename (Exchange i j) = mempty
rename (Partner a b)  = Rename $ \(Perm map) -> let
  invMap = Map.fromList $ fmap swap $ Map.toList map
  a' = invMap Map.! ctoi a
  b' = invMap Map.! ctoi b
  in Perm $ foldr (uncurry Map.insert) map [(a',ctoi b),(b',ctoi a)]

-- Compute the rename and the permutation across all moves
computePerms :: [DanceMove] -> (Rename,Perm)
computePerms ms = mconcat $ map (\m -> (rename m, perm m)) ms

renderPerm :: Perm -> String
renderPerm (Perm p) = fmap (itoc . (p Map.!)) range

------------ PART A ------------
partA :: Input -> String
partA moves = let
    (Rename rename, perm) = computePerms moves
  in renderPerm $ rename mempty <> perm

------------ PART B ------------
partB :: Input -> (String)
partB moves = let
    (Rename rename, perm) = computePerms moves
    fullRename = stimes 1_000_000_000 rename
    fullPerm   = stimes 1_000_000_000 perm
    in renderPerm $ fullRename mempty <> fullPerm
