 -- | n개의 원소 중 m개를 고르는 모든 조합을 찾는 알고리즘
  
 import Data.List
 import Data.Void

 main :: IO()
 main = do
     putStrLn $ "----------------------------------------------------"
     putStr   $ "기본 리스트를 입력하세요 = "
     input'list <- getLine
     putStr   $ "뽑을 개수를 입력하세요 = "
     m <- getLine
     putStrLn $ "----------------------------------------------------"
     putStrLn $ ""
     putStrLn $ input'list ++ " 에서 " ++ m ++  " 개를 뽑는 조합 찾기"
     putStrLn $ ""
     putStrLn $ "----------------------------------------------------"
     putStrLn $ ""
     result'list <- return $ exec'combi (read input'list) $ read m
     putStrLn $ "조합의 출력 >> " ++ "\n" ++ show result'list
     putStrLn $ ""
     putStrLn $ "----------------------------------------------------"


 -- | 조합의 수
 count'combi :: Int -> Int -> Int
 count'combi n m
     | n == m          = 1
     | n < 0 || m < 0  = 0
     | n < m           = 0
     | otherwise       = n * count'combi (n - 1) m


 -- | 전체 원소집합에서 n개의 조합을 찾아 출력하는 함수
 exec'combi :: [Int] -> Int -> [[Int]]
 exec'combi lists@(l:ls) m
     | length lists <  m  = [[   ]]
     | length lists == m  = [lists]
     | m == 1             = [[x] | x <- lists ]
     | otherwise          = fix'head l ls (m-1) ++ exec'combi ls m

 -- | 헤드를 반드시 포함하는 조합을 찾는 함수//  head ++ [n-1개를 뽑아서 만든 조합]
 fix'head :: Int -> [Int] -> Int -> [[Int]]
 fix'head fix tails@(t:ts) count
     | length tails <= count  = [ [fix] ++ tails ]
     | otherwise              = [ fix : x | x <- picked ] ++ fix'head fix ts count
     where picked = pick'tails tails count

 -- | n-1개에 대한 조합을 생성하는 함수
 pick'tails :: [Int] -> Int -> [[Int]]
 pick'tails scope count
     | length scope == count  = [scope]
     | otherwise              = [picked ++ [select] | select <- option]
     where
         point  = count-1
         picked = take point scope
         option = drop point scope
