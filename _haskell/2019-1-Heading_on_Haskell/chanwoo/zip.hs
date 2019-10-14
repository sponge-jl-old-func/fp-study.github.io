do_zip :: [a] -> [(a,a)]
do_zip xs = zip xs (tail xs)
