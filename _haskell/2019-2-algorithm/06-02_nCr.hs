-- | n개의 원소 중 m개를 고르는 모든 조합을 찾는 알고리즘

import Data.List
import Data.Void

main :: IO()
main = do
    putStrLn $ ""
    putStrLn $ "n개의 원소 중 m개를 고르는 모든 조합을 찾는 알고리즘"
    putStrLn $ "===================================================="
    
    putStr   $ "n = "
    input'n <- getLine
    putStr   $ "m = "
    input'm <- getLine
    putStrLn $ "----------------------------------------------------"
    putStrLn $ "n = " ++ input'n ++ " / " ++ " m = " ++ input'm 
    putStrLn $ "===================================================="
    putStrLn $ ""
    
    result <- return $ count'combi (read input'n) (read input'm)
    putStrLn $ "조합의 수 : " ++ show result ++ "\n"
    putStrLn $ "----------------------------------------------------"
    
    putStrLn $ ""
    putStrLn $ "[1,2,3,4,5] 에서 3개를 뽑는 조합 찾기 데모"
    putStrLn $ ""
    putStrLn $ "----------------------------------------------------"
    
    putStrLn $ ""
    result'list <- return $ extrc'combi [1,2,3,4,5,6] 3 
    putStrLn $ "조합의 출력 >> " ++ "\n" ++ show result'list ++ "\n"
    putStrLn $ ""


-- | 조합의 수
count'combi :: Int -> Int -> Int
count'combi n m
    | n == m          = 1
    | n < 0 || m < 0  = 0
    | n < m           = 0
    | otherwise       = n * count'combi (n - 1) m


-- | Lv.1 | 원소팩을 지정개수로 조합
extrc'combi :: [Int] -> Int -> [[Int]]
extrc'combi pack@(p:ps) num
    | num <= 0          = [[]]    -- 선택없음
    | check' pack num   = [pack]  -- 원소팩 전체 선택              
    | otherwise         =
            head'resrv pack num   -- [>>] 원소팩의 헤드 고정 조합 생성
            ++ extrc'combi ps num -- [>>] 원소팩의 헤드 제외 후 재귀

-- | Lv.2 | 원소팩의 헤드 고정하여 조합
head'resrv :: [Int] -> Int -> [[Int]]
head'resrv pack@(a:as) num =           -- 앵커(a) : 원소팩의 헤드
    let result = pick'tails as $ num-1 -- 결과조합: 원소팩의 테일로 만든 조합
    in  [a : rs | rs <- result]        -- [>>] 앵커와 결과조합을 최종조합

-- | Lv.3 | 앵커를 뺀 원소팩의 테일조합
pick'tails :: [Int] -> Int -> [[Int]]
pick'tails tail@(t:ts) num
    | check' tail num  = [tail]       -- 원소팩의 길이와 지정개수가 동일하면 팩 리턴
    | otherwise        =
        let scope = num - 1           -- 고정 크기
            prev  = take scope tail   -- 고정 구간
            loop  = drop scope tail   -- 변화 구간
        in  [prev ++ [x] | x <- loop] -- [>>] 변화 구간에서 추출한 원소를 고정구간과 조합
            ++ pick'tails ts num      -- [>>] 원소팩의 테일을 재귀호출


-- | util | 원소팩의 길이와 지정개수 비교
check' :: [Int] -> Int -> Bool
check' pack num =
    if (length pack == num) then True else False
