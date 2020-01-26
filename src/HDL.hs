module HDL where

import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)
import Data.List

data WireType = Internal | Input | Output
  deriving (Eq, Show)
type Wire = (Word, WireType)

data PrimGate = NAND Wire Wire Wire
  deriving Show

type MonadHDL m = (MonadState Word m, MonadWriter [PrimGate] m)

wire :: MonadState Word m => WireType -> m Wire
wire t = do
  w <- get
  modify (+1)
  pure (w, t)

gate :: MonadWriter [PrimGate] m => PrimGate -> m ()
gate n = tell [n]

nand :: MonadHDL m => Wire -> Wire -> Wire -> m ()
nand a b o = gate (NAND a b o)

notGate :: MonadHDL m => Wire -> Wire -> m ()
notGate i o = nand i i o

data WireState = High | Low | Undefined
  deriving (Eq, Show)

data RunState = RunState
  { _wires :: Map Word WireState
  }
  deriving Show
makeLenses ''RunState

type Circuit = [PrimGate]

runHDL :: StateT Word (Writer [PrimGate]) () -> Circuit
runHDL m = snd $ runWriter (void (runStateT m 0))

getWire :: Map Word WireState -> Wire -> WireState
getWire wires (w,_) = maybe Undefined id (M.lookup w wires)

setWire :: Wire -> WireState -> (Word, WireState)
setWire w s = set _2 s w

runPrimGate :: Map Word WireState -> PrimGate -> Map Word WireState
runPrimGate wires (NAND a b o) = M.fromList [setWire a a', setWire b b', setWire o o']
  where
    a' = getWire wires a
    b' = getWire wires b
    o' = case (a', b') of
           (High, High) -> Low
           (_, Low) -> High
           (Low, _) -> High
           (_, _) -> Undefined

exampleCircuit :: Circuit
exampleCircuit = runHDL $ do
  i <- wire Input
  a <- wire Internal
  o <- wire Output
  notGate i a
  notGate a o

tick :: MonadState RunState m => Circuit -> m ()
tick gates = do
  s <- get
  modify (set wires (M.unions $ fmap (runPrimGate (s^.wires)) gates))

run :: Circuit -> [WireState] -> [[WireState]]
run c inVals = output
  where
    getWires (NAND a b c) = [a, b, c]
    allWires = nubBy (\a b -> fst a == fst b) $ concatMap getWires c
    inWires = fst <$> filter (\(w, s) -> s == Input) allWires
    outWires = filter (\(w, s) -> s == Output) allWires
    initialState = M.fromList (zip inWires inVals)
    running = do
      tick c
      s <- get
      let outVals = map (getWire (s^.wires)) outWires
      tell [outVals]
    output = execWriter $ execStateT (forever running) (RunState initialState)
