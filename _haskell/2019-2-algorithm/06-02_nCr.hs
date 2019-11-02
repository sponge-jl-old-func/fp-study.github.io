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
     | otherwise          = fix'head l ls (m-1) -- 헤드를 반드시 포함하는 조합 작성
                            ++ exec'combi ls m  -- 재귀호출

 -- | head를 반드시 포함하는 조합 작성 함수
 -- | head : [n-1개를 뽑아서 만든 조합]
 fix'head :: Int -> [Int] -> Int -> [[Int]]
 fix'head fix tails@(t:ts) count
     | length tails <= count  = [ [fix] ++ tails ]
     | count == 1             = [ fix :[x]| x <- tails ] -- 헤드 외에 1개만 뽑을 때 재귀없이 간단작성
     | otherwise              = [ fix : x | x <- set ]   -- 헤드와 조합을 매칭
                                ++ fix'head fix ts count -- 재귀호출
     where
        set = pick'tails tails count -- n-1 개를 선택하여 작성된 조합

 -- | n-1개를 선택하여 조합 작성
 pick'tails :: [Int] -> Int -> [[Int]]
 pick'tails scope count                 -- n-1 개를 입력으로 받음
     | length scope == count  = [scope] -- 길이와 개수가 같으면 그대로 반환
     | otherwise              = [picked ++ [select] | select <- option] -- 고정과 선택 조합
     where
         point  = count-1
         picked = take point scope -- 지점까지 고정할 부분
         option = drop point scope -- 지점이후 선택할 부분
