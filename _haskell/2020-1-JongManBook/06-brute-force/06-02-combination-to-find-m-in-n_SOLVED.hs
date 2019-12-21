-- | [JongManBook] Algorithm Problem Solving Strategies (book-1)
-- |  06. 무식하게 풀기
-- |  02. n개의 원소 중 m개를 고르는 모든 조합을 찾는 알고리즘

-- | n: 전체 원소의 수
-- | picked: 지금까지 고른 원소들의 번호
-- | toPick: 앞으로 고를 원소의 수

-- | 목표 : 앞으로 toPick개의 원소를 고르는 모든 방법을 출력한다.
-- | 제한 : m에 상관없이 중첩반복문을 사용하지 않을 것
-- | 함수 : void pick(int n, vector<int>& picked, int toPick)
-- |  - 기저사례: 더 고를 원소가 없을 때 고른 원소들을 출력한다.
-- |  - 고를 수 있는 가장 작은 번호를 계산한다.
-- |  - 이 단계에서 원소를 고른다.

import Control.Monad

pick :: Int -> [Int] -> Int -> IO()
pick _ picked 0      = print $ picked
pick n picked toPick = forM_ [smallest..n] $ \next -> pick n (picked ++ [next]) (toPick-1)
  where
      smallest = case picked of
                  [] -> 0
                  otherwise -> (last picked) + 1

main :: IO()
main = pick n [] toPick
  where 
    n = 5
    toPick = 3