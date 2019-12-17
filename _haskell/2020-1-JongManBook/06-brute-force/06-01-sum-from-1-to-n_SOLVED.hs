-- | [JongManBook] Algorithm Problem Solving Strategies (book-1)
-- |  06. 무식하게 풀기
-- |  01. 1부터 n까지 합을 계산하는 반복함수와 재귀함수

main :: IO()
main = do

  putStrLn $ ""
  putStrLn $ "==================================================="
  putStrLn $ " 06. 무식하게 풀기"
  putStrLn $ " 01. 1부터 n까지 합을 계산하는 반복함수와 재귀함수"
  putStrLn $ "==================================================="

  putStr   $ " n = "
  inputStr <- getLine
  outputInt<- return $ recursive $ read inputStr
  putStrLn $ " --> " ++ show outputInt
  putStrLn $ "==================================================="



-- | 조건: n >= 1
-- | 결과: 1부터 n까지의 합을 반환한다.
-- | 함수: int sum(int n)
-- | >> not need

-- | 조건: n >= 1
-- | 결과: 1부터 n까지의 합을 반환한다.
-- | 함수: int recursive(int n)
recursive :: Int -> Int
recursive n
  | n <= 0    = 0
  | otherwise = n + recursive (n-1)
