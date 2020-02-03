-- | [JongManBook] Algorithm Problem Solving Strategies (book-1)
-- |  06. 무식하게 풀기
-- |  04. 보글 게임(Boggle Game) : https://algospot.com/judge/problem/read/BOGGLE


-- | 게임판
board :: [[Char]]
board = [ "URLPM"
        , "XPRET"
        , "GIAET"
        , "XTNZY"
        , "XOQRS" ]


-- | 좌표값과 문자열 탐색 함수
hasWord :: Int -> Int -> String -> Bool
hasWord y x word@(w:ws) 
  | outOfRange y x             = False
  | board!!y!!x /= w           = False
  | length word == 1           = True  -- | 기저 사례
  | search $ \(dx, dy) -> 
      hasWord (y+dy) (x+dx) ws = True  -- | 재귀 호출
  | otherwise                  = False


-- | 좌표 유효성 탐색 함수
outOfRange :: Int -> Int -> Bool
outOfRange y x = not $ 0 <= x && x < 5 && 0 <= y && y < 5


-- | 탐색 함수 
search :: ((Int, Int) -> Bool) -> Bool
search = flip any $ filter (/=(0, 0)) $ (,) <$> [-1, 0, 1] <*> [-1, 0, 1]


-- | flip :: (a -> b -> c) -> b -> a -> c
-- | any :: (a -> Bool) -> [a] -> Bool
-- | filter :: (a -> Bool) -> [a] -> [a]
-- | (<$>) :: Functor f => (a->b) -> f a -> f b
-- | (<*>) :: f (a -> b) -> f a -> f b

-- | 하스켈에서 `[](List)`는 Functor 입니다.
-- | `<$>` 연산자를 통해 `,` 연산자를 펑터(함수)인 리스트(`[]`) 펑터와 매핑하면
-- | [(-1,), (0,), (1,)]이 반환됩니다.
-- | 이 함수를 `<*>` 연산자가 받으면 펑터(함수)인 리스트(`[]`)와 매핑하여
-- | [(-1,) [-1,0,1], (0,) [-1,0,1], (1,) [-1,0,1]] 이 반환됩니다.
-- | 결과적으로 [(-1,-1), ... (1,1)]로 튜플리스트를 반환합니다.
-- | 튜플리스트는 다시 filter 함수를 통해 (0,0)을 제외하여 필터된 튜플리스트가 됩니다.
-- | 결과적으로 search함수는 `filp any [(-1,-1), ... (1,1)]`의 함수를 반환합니다.

-- | hasWord 함수의 가드에서 호출된 search함수가 람다를 전달받습니다.
-- | flip함수에 의해 `any 람다함수 [(-1,-1), ... (1,1)]` 가 되고,
-- | any함수는 리스트의 각 인덱스인 튜플을 람다함수의 입력으로 전달하면
-- | (0,0) 즉 현재 위치를 제외한 8방향에 대한 재귀호출을 합니다.
-- | 재귀호출의 끝에 함수가 풀리기 시작하면 any 함수에 의해
-- | 한번이라도 False가 나온 경우 제귀가 끝나고 다음 방향을 탐색합니다.

