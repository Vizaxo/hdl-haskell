module HDL where

import Control.Monad.State
import Control.Monad.Writer

type Wire = Word

data PrimGate = NAND Wire Wire Wire

type MonadHDL m = (MonadState Wire m, MonadWriter [PrimGate] m)

wire :: MonadState Wire m => m Wire
wire = do
  w <- get
  modify (+1)
  pure w

gate :: MonadWriter [PrimGate] m => PrimGate -> m ()
gate n = tell [n]

nand :: MonadHDL m => Wire -> Wire -> Wire -> m ()
nand a b o = do
  gate (NAND a b o)
  pure ()

not :: MonadHDL m => Wire -> Wire -> m ()
not i o = do
  nand i i o
  pure ()
