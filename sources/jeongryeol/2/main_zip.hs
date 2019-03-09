
module Main where

do_zip :: [a] -> [(a,a)]
do_zip xs = zip xs (tail xs)

main = do
	let result = do_zip 1 2
	print result

