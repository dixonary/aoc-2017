module Days.Day10 (runDay, hash, applyHash) where

{- ORMOLU_DISABLE -}
import           Data.List
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as Vec
import qualified Util.Util                       as U

import           Control.Applicative.Combinators hiding (sepBy)
import           Data.Attoparsec.Combinator      (lookAhead)
import           Data.Attoparsec.Text            hiding (take)
import           Data.Void
import qualified Program.RunDay                  as R (runDay)

import           Control.Monad.State.Strict
import qualified Data.List.PointedList           as Tape
import           Data.List.PointedList.Circular  as CTape hiding (length)

import           Data.Bifunctor                  (first, second)
import           Data.Bits                       (xor)
import           Data.Char                       (ord)
import           Data.Function                   ((&))
import           Data.Functor
import           Debug.Trace
import           Text.Printf                     (printf)

import Data.STRef
import Control.Monad.ST
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MVec

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,)
          <$> lookAhead (decimal `sepBy` char ',')
          <*> many (notChar '\n')


------------ TYPES ------------
type Input = ([Int], String)

type CTape = PointedList Int

-- Perform the required twist.
reverseElems :: forall s . STVector s Int -> Int -> Int -> Int -> ST s ()
reverseElems vec len fromPos twistAmt =
  () <$ sequence
    [ MVec.unsafeSwap vec old new
    | x <- [ 0..twistAmt `div` 2 - 1 ]
    , let old = (fromPos + x) `mod` len
    , let new = (fromPos + twistAmt - 1 - x) `mod` len
    ]

applyHash :: Int -> [Int] -> Vector Int
applyHash rounds lengths =
  let hashWidth = 256
      vraw = Vec.fromList [0..hashWidth-1]
  in runST $ do
      v <- Vec.thaw vraw
      offset <- newSTRef 0
      skipVal <- newSTRef 0
      replicateM_ rounds $ do
        forM_ lengths $ \length -> do
          o <- readSTRef offset
          s <- readSTRef skipVal
          reverseElems v hashWidth o length
          modifySTRef offset (\x -> (x + length + s) `mod` hashWidth)
          modifySTRef skipVal (+ 1)
      Vec.freeze v

hash :: String -> String
hash input = input
   & fmap ord
   & (++ [17, 31, 73, 47, 23])
   & applyHash 64
   & Vec.toList
   & U.chunksOf 16
   & map (foldl1 xor)
   & map (printf "%02x")
   & concat


-- hash :: String -> String
-- hash input = replicateM 64 (mapM twist (fmap ord input ++ [17, 31, 73, 47, 23]))
--     & flip execState (0, makeCTape [0..255])
--     & snd
--     & tapeToList
--     & U.chunksOf 16
--     & map (foldl1 xor)
--     & map (printf "%02x")
--     & concat

-- twist :: Int -> State (Int,CTape) ()
-- twist len = do
--     skipVal <- gets fst
--     tape    <- gets snd
--     let rawTape = tapeToList tape
--         offset  = index tape
--
--         (selection,rest) = splitAt len $ rotate offset $ rawTape
--         tape' = rotate (length rawTape - offset) $ reverse selection ++ rest
--
--     setTape
--         $ (reverse selection ++ rest)
--         & rotate (length rawTape - offset)
--         & makeCTape
--         & moveTo' offset
--         & moveN len
--         & moveN skipVal
--
--     incrementSkipVal
--     return ()

-- rotate n ls      = let (ys,xs) = splitAt n ls in xs++ys

-- Partial move and CTape creation (use on nonempty lists only)
-- moveTo' n        = fromJust . Tape.moveTo n
-- makeCTape        = fromJust . CTape.fromList
-- incrementSkipVal = modify $ first (+1)
-- setTape t        = modify $ second $ const t
--
-- fullTape :: State (Int,CTape) [Int]
-- fullTape         = gets (tapeToList . snd)
-- tapeToList PointedList{..} = reverse _reversedPrefix ++ [_focus] ++ _suffix

------------ PART A ------------
partA :: Input -> Int
-- partA (input,_) = mapM twist input
--                & flip execState (0, makeCTape [0..255])
--                & snd
--                & tapeToList
--                & \(x:y:_) -> x * y
partA (input,_) = applyHash 1 input
                & Vec.toList
                & \(x:y:_) -> x * y


------------ PART B ------------
partB :: Input -> String
partB = hash . snd

