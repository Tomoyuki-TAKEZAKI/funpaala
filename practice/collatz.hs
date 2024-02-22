import Data.Bool (bool)

takeTo :: (a -> Bool) -> [a] -> [a]

-- 素朴に再帰で書く
-- takeTo _ [] = []
-- takeTo p (x : xs)
--         | p x = [x]
--         | otherwise = x : takeTo p xs

-- ヘルパー関数の導入
-- takeTo p (x : xs) = fun x (takeTo p xs)
        -- where fun x lst = if p x then [x] else x : lst

-- 畳み込みとして書ける
-- takeTo p = foldr (\x lst -> bool (x : lst) [x] (p x)) []

-- lst を使わず、さらに x : の重複を削除
-- takeTo p = foldr (\x -> (x :) . bool id (const []) (p x)) []

-- やりすぎ
takeTo = (`foldr` []) . ((.) <$> (:) <*>) . (bool id (const []) .)

collatzNext :: Integer -> Integer
collatzNext n
        | even n = n `div` 2
        | otherwise = 3 * n + 1

collatzInf :: Integer -> [Integer]
-- collatzInf = iterate collatzNext
collatzInf = iterate $ \n -> bool (3 * n + 1) (n `div` 2) (even n)

collatz :: Integer -> [Integer]
collatz = takeTo (== 1) . collatzInf
