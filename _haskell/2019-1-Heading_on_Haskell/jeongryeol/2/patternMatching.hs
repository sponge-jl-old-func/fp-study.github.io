exPatMat :: Int -> Int -> Int -> Int
exPatMat 0 1 0 = 999       -- Arg. Pattern
exPatMat 1 1 1 = 1         -- Arg. Pattern
exPatMat x y z = x + y + z -- otherwise..
