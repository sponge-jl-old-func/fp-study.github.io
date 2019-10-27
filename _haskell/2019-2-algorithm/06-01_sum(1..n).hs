-- 1부터 n까지의 합을 구하는 재귀함수

main :: IO()
main = do 
  putStrLn $ "1부터 n까지의 합을 구하는 재귀함수"
  putStrLn $ "----------------------------------"
  putStrLn $ "자연수 n을 입력하시오."
  input <- getLine
  putStrLn $ "----------------------------------"
  putStrLn $ "입력한 숫자 = " ++ show input
  result <- return $ sum'to'n $ read input
  putStrLn $ "=================================="
  putStrLn $ "결과값 = " ++ show result ++ "\n"

sum'to'n :: Int -> Int
sum'to'n x
  |  x <  0    = -1 
  |  x == 0    = 0
  |  otherwise = x + sum'to'n (x-1)
