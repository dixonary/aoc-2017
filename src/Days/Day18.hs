module Days.Day18 (runDay) where

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

import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MVec
import Control.Monad.ST
import Data.STRef
import Control.Applicative
import Data.Char (ord)
import Data.Functor
import Control.Monad
import Debug.Trace
import Control.Monad.Loops
import Control.Monad.RWS


{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    ident = (Ref <$> letter) <|> (Val <$> signed decimal)
    r  iname c = iname >> space >> (c <$> letter)
    v  iname c = iname >> space >> (c <$> ident)
    rv iname c = iname >> space >> (c <$> letter <*> (space >> ident))
    vv iname c = iname >> space >> (c <$> ident  <*> (space >> ident))
    instruction = choice
      [ v  "snd" Snd
      , rv "set" Set
      , rv "add" Add
      , rv "mul" Mul
      , rv "mod" Mod
      , r  "rcv" Rcv
      , vv "jgz" Jgz
     ]
  in Vec.fromList <$> instruction `sepBy1` endOfLine


------------ TYPES ------------
type Input = Program
type Program = Vector Instruction
type Register = Char

data Memory = Memory
  { input       :: [Integer]
  , registers   :: Vector Integer
  , iPtr        :: Integer
  , recvRoutine :: Register -> Computer Status
  }

type Computer a = RWS
    Program   -- Reader (environment) = instruction set
    [Integer] -- Writer (output)      = sent data
    Memory    -- State  (memory)
    a

data Expr = Ref Register | Val Integer
  deriving (Show)

data Instruction
  = Snd Expr
  | Set Register Expr
  | Add Register Expr
  | Mul Register Expr
  | Mod Register Expr
  | Rcv Register
  | Jgz Expr Expr
  deriving (Show)

data Status = Stop | Continue
  deriving (Show,Eq)


-- Shorthand helpers for common sub-operations
send = tell . pure
movePtr n = modify (\x@Memory{..} -> x { iPtr = iPtr + n })
advance = movePtr 1 >> return Continue

-- Modify registers --
setReg c v = let
  i = ord c - ord 'a'
  in modify (\x -> x { registers = registers x Vec.// [(i,v)] } )

getReg c = let
  i = ord c - ord 'a'
  in gets ((Vec.! i) . registers)

adjustReg a f b = do
  a' <- getReg a
  b' <- getVal b
  setReg a $ a' `f` b'

-- Evaluate an expression --
getVal :: Expr -> Computer Integer
getVal (Val c) = return c
getVal (Ref r) = getReg r

-- Interpret an instruction --
interpret :: Instruction -> Computer Status
interpret (Snd va)    = getVal va >>= send      >> advance
interpret (Set ra vb) = getVal vb >>= setReg ra >> advance
interpret (Add ra vb) = adjustReg ra (+) vb     >> advance
interpret (Mul ra vb) = adjustReg ra (*) vb     >> advance
interpret (Mod ra vb) = adjustReg ra mod vb     >> advance
interpret (Rcv ra)    = gets recvRoutine >>= ($ ra)
interpret (Jgz va vb) = do
  a' <- getVal va
  b' <- getVal vb
  movePtr $ if a' > 0 then b' else 1
  return Continue

-- In part A, we stop as soon as we read a
-- nonzero value from register ra.
rcvPartA :: Register -> Computer Status
rcvPartA ra = do
  a' <- getReg ra
  if (a' /= 0)
  then return Stop
  else advance

-- In part B, we stop when the input buffer is empty.
rcvPartB :: Register -> Computer Status
rcvPartB ra = do
  x <- gets input
  case x of
    [] -> return Stop
    (x:xs) -> do
      setReg ra x
      modify $ \m@Memory{..} -> m{input = xs}
      advance

-- Perform a single computational step.
step :: Computer Status
step = do
  i <- gets iPtr
  programLength <- asks $ fromIntegral . length
  if (i < 0 || i >= programLength)
    then return Stop
    else do
      instr <- asks (Vec.! fromIntegral i)
      res <- interpret instr
      return res

-- Run the computer until it can proceed no further.
runChunk :: Program -> Memory -> (Status,Memory,[Integer])
runChunk = runRWS $ iterateWhile (== Continue) step


------------ PART A ------------
partA :: Input -> Integer
partA program = let
  initialReg = Vec.fromList $ map (const 0) [0..25]
  state = Memory output initialReg 0 rcvPartA
  (Stop,_,output) = runChunk program state
  in last output


------------ PART B ------------
partB :: Input -> Int
partB program = runST $ do
    let initialReg0 = Vec.fromList $ map (const 0) [0..25]
    let initialReg1 = initialReg0 Vec.// [(15,1)]
    memoryA <- newSTRef (Memory [] initialReg0 0 rcvPartB)
    memoryB <- newSTRef (Memory [] initialReg1 0 rcvPartB)
    totalOutputB <- newSTRef 0

    let
      runMachine memRef = do
        mem <- readSTRef memRef
        let (Stop,mem',out') = runChunk program mem
        writeSTRef memRef mem'
        return out'

      runMachines = do
        -- Run A, then plug results into B
        outA <- runMachine memoryA
        modifySTRef memoryB $ \m -> m{ input = input m ++ outA }
        -- Run B, then plug results into A
        outB <- runMachine memoryB
        modifySTRef memoryA $ \m -> m{ input = input m ++ outB }
        -- Store total outputs of B, for propsperity
        modifySTRef totalOutputB (+ length outB)
        -- Return combined outputs
        return $ outA ++ outB

    iterateUntil null runMachines
    readSTRef $ totalOutputB
