module Days.Day08 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where 
    instruction = do
      register <- many1 letter
      string " "
      change <- do
        op <- choice [string "inc" >> pure (+), string "dec" >> pure (-)]
        string " "
        amt <- signed decimal
        return (`op` amt)
      string " if "
      refReg <- many1 letter
      string " "
      refProp <- do
        op' <- choice $ map (\(a,b) -> string a >> pure b)
              [ (">=",(>=))
              , ("<=",(<=))
              , (">",(>))
              , ("<",(<))
              , ("==",(==))
              , ("!=",(/=))
              ]
        string " "
        refValue <- signed decimal
        return (`op'` refValue)
      return Instruction {..}


------------ TYPES ------------
type Input = [Instruction]

data Instruction = Instruction
  { register  :: String
  , change    :: Int -> Int
  , refReg    :: String
  , refProp   :: Int -> Bool
  }
instance Show Instruction where show Instruction{..} = "Instr on " ++ register

------------ PART A ------------
partA :: Input -> Int
partA = maximum . Map.elems . foldl' line Map.empty

line :: Map String Int -> Instruction -> Map String Int
line m Instruction{..} = 
  if refProp $ Map.findWithDefault 0 refReg m 
  then Map.adjust change register $ Map.insertWith (flip const) register 0 m
  else m

------------ PART B ------------
partB :: Input -> Int
partB = maximum' . fmap (maximum' . Map.elems) . scanl' line Map.empty
  where 
    maximum' [] = 0
    maximum' ls = maximum ls