module BooleanLogic where

import HDL
import Testing

andGate :: MonadHDL m => Wire -> Wire -> Wire -> m ()
andGate a b o = do
  x <- wire Internal
  nand a b x
  notGate x o

testAnd = test c table where
  c = runHDL $ do
        a <- wire Input
        b <- wire Input
        o <- wire Output
        andGate a b o
  table :: TestTable
  table =
    [([High,High],[High])
    ,([High,Low],[Low])
    ,([Low,High],[Low])
    ,([Low,Low],[Low])
    ]

orGate :: MonadHDL m => Wire -> Wire -> Wire -> m ()
orGate a b o = do
  a' <- wire Internal
  b' <- wire Internal
  notGate a a'
  notGate b b'
  nand a' b' o

testOr = test c table where
  c = runHDL $ do
        a <- wire Input
        b <- wire Input
        o <- wire Output
        orGate a b o
  table :: TestTable
  table =
    [([High,High],[High])
    ,([High,Low],[High])
    ,([Low,High],[High])
    ,([Low,Low],[Low])
    ]

xorGate :: MonadHDL m => Wire -> Wire -> Wire -> m ()
xorGate a b o = do
  na <- wire Internal
  nb <- wire Internal
  c <- wire Internal
  d <- wire Internal
  notGate a na
  notGate b nb
  andGate a nb c
  andGate b na d
  orGate c d o

testXor = test c table where
  c = runHDL $ do
        a <- wire Input
        b <- wire Input
        o <- wire Output
        xorGate a b o
  table :: TestTable
  table =
    [([High,High],[Low])
    ,([High,Low],[High])
    ,([Low,High],[High])
    ,([Low,Low],[Low])
    ]
