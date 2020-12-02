module Days.Day10 (runDay) where

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

import           Control.Monad.State
import qualified Data.List.PointedList           as Tape
import           Data.List.PointedList.Circular  as CTape hiding (length)

import           Data.Bifunctor                  (first, second)
import           Data.Bits                       (xor)
import           Data.Char                       (ord)
import           Data.Function                   ((&))
import           Data.Functor
import           Debug.Trace
import           Text.Printf                     (printf)

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,)
          <$> lookAhead (decimal `sepBy` char ',')
          <*>(fmap (init . fmap ord) (many anyChar))


------------ TYPES ------------
type Input = ([Int], [Int])

type CTape = PointedList Int

twist :: Int -> State (Int,CTape) ()
twist len = do
    skipVal <- gets fst
    tape    <- gets snd
    let rawTape = tapeToList tape
        offset  = index tape

        (selection,rest) = splitAt len $ rotate offset $ rawTape
        tape' = rotate (length rawTape - offset) $ reverse selection ++ rest

    setTape
        $ (reverse selection ++ rest)
        & rotate (length rawTape - offset)
        & makeCTape
        & moveTo' offset
        & moveN len
        & moveN skipVal

    incrementSkipVal
    return ()

rotate n ls      = let (ys,xs) = splitAt n ls in xs++ys

-- Partial move and CTape creation (use on nonempty lists only)
moveTo' n        = fromJust . Tape.moveTo n
makeCTape        = fromJust . CTape.fromList
incrementSkipVal = modify $ first (+1)
setTape t        = modify $ second $ const t

fullTape :: State (Int,CTape) [Int]
fullTape         = gets (tapeToList . snd)
tapeToList PointedList{..} = reverse _reversedPrefix ++ [_focus] ++ _suffix

------------ PART A ------------
partA :: Input -> Int
partA (input,_) = mapM twist input
                & flip execState (0, makeCTape [0..255])
                & snd
                & tapeToList
                & \(x:y:_) -> x * y


------------ PART B ------------
partB :: Input -> String
partB (a,input) = replicateM 64 (mapM twist (input ++ [17, 31, 73, 47, 23]))
                & flip execState (0, makeCTape [0..255])
                & snd
                & tapeToList
                & traceShowId
                & U.chunksOf 16
                & map (foldl1 xor)
                & map (printf "%02x")
                & concat
