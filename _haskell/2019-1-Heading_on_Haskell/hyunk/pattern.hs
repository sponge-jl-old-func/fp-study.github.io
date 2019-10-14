module Main where

list_pattern :: Int -> Int -> String

list_pattern a b | a + b >= 0 = "positive number"
		 | a + b < 0 = "negative number"
		 | a + b == 0 = "zero"

main = do
	let result = list_pattern 10 20
	print result
