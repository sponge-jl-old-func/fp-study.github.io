count :: Eq a => a -> [a] -> Int
count x xl = length [ x' | x' <- xl , x == x' ]
