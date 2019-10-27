odds n = map f [0..n]
          where
	    f x = x `mod` 2 /= 0

odds1 n = map (\x -> x `mod` 2 /= 0) [0..n]
