
-- Example #1 : TODO Program on Haskell

week = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
month = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct", "Nov", "Dec"]
days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

main :: IO()
main = do
	putStrLn "Calendar"
	mapM_ print_month (zip month days)


print_month :: (String, Int) -> IO()
print_month (month, days) = do
	putStrLn $ month 
	putStrLn (join_week week)
	print_day days ""
	putStrLn "\n"

print_day :: Int -> String -> IO()
		
print_day d prt | d > 0 = print_day (d-1) ((show d) ++ " " ++ prt)
		| otherwise = putStrLn(prt)


join_week :: [String] -> String
join_week week = foldr (\w1 w2 -> w1 ++ " " ++  w2) "" week






