-- | [JongManBook] Algorithm Problem Solving Strategies (book-1)
-- |  06. 무식하게 풀기
-- |  03. 소풍

-- | 유치원 원내 행사를 위하여 2인 1조로 짝을 지어 소풍을 보내고자 한다.
-- | 단, 항상 서로 친구인 학생끼리만 짝을 지어야 한다.
-- | 짝을 짓는 방법의 수를 찾을 때, 다음과 같은 경우는 서로 다른 방법이다.
-- | * (A, B) (C, D) (E, F)
-- | * (A, B) (C, E) (D, F)

-- | << 입력
-- | L1 : 테스트 케이스 횟수 C (C ≤ 50)
-- |      학생의 수 n (2 ≤ n ≤ 10 을 만족하는 짝수)
-- |      서로 친구인 쌍의 수 m ( 0 ≤ m ≤ n(n-1)/2 )
-- | L2 : m개의 정수 쌍으로 서로 친구인 두 학생의 번호
-- |      ( 0부터 n-1 사이의 정수이고, 같은 쌍이 중복되어 입력되지 않음)

-- | >> 출력
-- | 각 테스트 케이스마다 한 줄에 모든 학생을 친구끼리만 짝 지어줄 수 있는 방법의 수

import Control.Monad
import Data.List
import Debug.Trace

-- | Random-DB-1 : 끼리끼리 친한 임의친구 데이터
randomFriends :: [[Bool]]
randomFriends =
  [
    [ True,  True, False,  True, False,  True,  True,  True, False,  True], -- 0
    [ True,  True,  True, False,  True, False,  True, False, False, False], -- 1
    [False,  True,  True, False, False,  True, False, False, False,  True], -- 2
    [ True, False, False,  True,  True, False,  True, False,  True,  True], -- 3
    [False,  True, False,  True,  True,  True, False,  True, False, False], -- 4
    [ True, False,  True, False,  True,  True, False, False, False, False], -- 5
    [ True,  True, False,  True, False, False,  True,  True, False, False], -- 6
    [ True, False, False, False,  True, False,  True,  True, False, False], -- 7
    [False, False, False,  True, False, False, False, False,  True,  True], -- 8
    [ True, False,  True,  True, False, False, False, False,  True,  True]  -- 9
  ]

-- | Random-DB-2 : 모두가 친한 친구 데이터
allFriends :: [[Bool]]
allFriends = take 10 $ repeat $ take 10 (repeat True)

-- | DB 선택
areFriends = allFriends

-- | ####### 실행 #######
main :: IO()
main = do 
    print $ countPairings input
    where
        input = [False, False, False, False, False, False]

countPairings :: [Bool] -> Maybe Int
countPairings taken = do
  -- | taken : 짝맺기가 완료될 때마다 갱신될 친구 리스트 ([Bool])
  -- | firstFree : 아직 짝이 맺어지지 않은 가장 앞의 친구  
  let firstFree = elemIndex False taken
  case firstFree of
    -- | (기저사례) 모두 짝이 맺어지면 경우의 수 1개 반환
    Nothing -> return 1
    -- | (재귀수행) 짝이 맺어지지 않은 친구(A)에 대하여
    otherwise -> do
      firstFree' <- firstFree -- | unwrapping : Int <- Maybe Int
      -- | 그 다음 친구(B)부터 마지막 친구(n)까지 짝을 맺을 수 있는지 탐색(lambda do 구문)
      -- | 위 설명을 반복한 결과는 리스트에 담기고, 이 결과 리스트를 sum으로 매핑해서 모두 합산
      fmap sum $ forM [(firstFree'+1)..n] $ \pairWith -> do
        -- | (&& 좌항) pairWith 번째 차례의 친구가 아직 짝이 맺어지지 않았는 지 체크
        -- | (&& 우항) DB에서 조회한 A친구가 pairWith친구와 친구인지 체크
        case (not $ taken!!pairWith) && (areFriends!!firstFree'!!pairWith) of
          -- | (두 조건을 만족하지 목하면) 짝맺기가 불가능하므로 경우의 수 0개 반환
          False -> return 0
          -- | (두 조건을 모두 만족하면) 짝맺기가 가능하므로 두 친구를 짝 맺은 정보로 갱신하여 재귀 호출
          True -> countPairings [ if index == firstFree' || index == pairWith
                                    then True else orgTakenInfo
                                | (index, orgTakenInfo) <- zip [0..n] taken ]
  where 
    -- | 마지막 친구(n)
    n = (length taken) - 1
