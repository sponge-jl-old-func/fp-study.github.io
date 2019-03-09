do_zip :: [a] -> [(a,a)]
do_zip arg = zip arg (tail arg)

do_sorting :: Ord a => [a] -> [Bool] -- Bool
do_sorting arg2
	= --and 
		[ x <= y | ( x, y ) <- do_zip arg2 ]

