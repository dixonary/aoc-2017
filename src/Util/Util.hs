{-# LANGUAGE BangPatterns #-}
module Util.Util where

{- ORMOLU_DISABLE -}
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vec

import Control.Monad
{- ORMOLU_ENABLE -}

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : (attachCoords x (y + 1) (ls : lss))

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | length ls == 0 = []
  | length ls < n = [ls]
  | otherwise = (take n ls) : (chunksOf n (drop n ls))


data Repeat a = Repeat
    { value       :: a
    , firstIndex  :: Int
    , secondIndex :: Int
    }

-- Find the first time that an element repeats in a list.
-- Returns Nothing if no repeats were found.
firstRepeat :: Ord a => [a] -> Maybe (Repeat a)
firstRepeat xs = firstRepeat' xs Map.empty 0
  where
    firstRepeat' [] _ _ = Nothing
    firstRepeat' (x:xs) seen i =
      case Map.lookup x seen of
        Just i' -> pure $ Repeat x i' i
        Nothing -> firstRepeat' xs (Map.insert x i seen) (i+1)

-- We need this here, because containers on stackage is too old
-- and doesn't include `compose` :(

composeMaps :: Ord b => Map b c -> Map a b -> Map a c
composeMaps bc !ab
  | null bc = Map.empty
  | otherwise = Map.mapMaybe (bc Map.!?) ab

-- | Keep running an operation until it becomes 'False'.
whileM :: Monad m => m Bool -> m ()
whileM act = do
  b <- act
  when b $ whileM act

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f
