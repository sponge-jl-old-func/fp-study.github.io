
module Main where


my_add::Int -> Int -> Int
my_add 0 0 = -1
my_add a b = a + b


main = do
	let result = my_add 1 2
	print result

