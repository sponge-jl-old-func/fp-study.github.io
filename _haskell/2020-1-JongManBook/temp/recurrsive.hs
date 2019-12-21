
import Control.Monad
import Debug.Trace



-- Example 3: Bubble game
board = [
            "URLPM"
        ,   "XPRET"
        ,   "GIAET"
        ,   "XTNZY"
        ,   "XOQRS"
        ]


hasWord :: Int -> Int -> String -> Bool
hasWord y x word 
  | outOfRange y x                      = False
  | board!!y!!x /= head word            = False
  | length word == 1                    = True
  | search $ \(dx, dy) -> 
      hasWord (y+dy) (x+dx) (tail word) = True
  | otherwise                           = False

    where
        outOfRange y x = not $ 0 <= x && x < 5 && 0 <= y && y < 5
        search = (elem True) . (flip map $ zip dxs dys) 
        dxs = [-1, -1, -1, 1, 1, 1, 0, 0]
        dys = [-1,  0,  1,-1, 0, 1,-1, 1]


main3 :: IO ()
main3 = print $ hasWord 0 0 "URPAZRSYTTM"



-- Example 2: Combining all subsets

pick :: Int -> [Int] -> Int -> IO ()
pick _ picked 0 = print $ picked
pick n picked toPick = forM_ [smallest..n] $ \next -> pick n (picked ++ [next]) (toPick-1) 

    where
        smallest = case picked of 
                     [] -> 0
                     otherwise -> (last picked) + 1
                   

main :: IO ()
main = pick n [] toPick
    where
        n = 5
        toPick = 3


-- Example 1: Sum numbers (1~n)

sumOfNumbers :: Int -> Int
sumOfNumbers 0 = 0
sumOfNumbers n = n + sumOfNumbers (n-1)

main1 :: IO ()
main1 = print $ sumOfNumbers n
    where
        n = 100



