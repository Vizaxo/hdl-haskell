module Testing where

import HDL

type TestTable = [([WireState], [WireState])]

data Result
  = Pass
  | Fail { ins :: [WireState], expected :: [WireState], got :: [WireState] }
  deriving Show

result :: [WireState] -> [WireState] -> [WireState] -> Result
result ins expected got
  | expected == got = Pass
  | otherwise = Fail ins expected got

test :: Circuit -> TestTable -> [Result]
test c t = runTest <$> t
  where
    runTest :: ([WireState], [WireState]) -> Result
    runTest (ins, expected) = result ins expected ((run c ins) !! 10)

testNand = test c table where
  c = runHDL $ do
        a <- wire Input
        b <- wire Input
        o <- wire Output
        nand a b o
  table :: TestTable
  table =
    [([High,High],[Low])
    ,([High,Low],[High])
    ,([Low,High],[High])
    ,([Low,Low],[High])
    ]
