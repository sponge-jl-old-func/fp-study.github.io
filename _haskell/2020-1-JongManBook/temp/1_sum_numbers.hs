
-- Example 1: Sum numbers (1~n)

sumOfNumbers :: Int -> Int
sumOfNumbers 0 = 0
sumOfNumbers n = n + sumOfNumbers (n-1)

main :: IO ()
main = print $ sumOfNumbers n
    where
        n = 100





