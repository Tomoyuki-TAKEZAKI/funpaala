import Data.List (unfoldr)
import Data.Bool (bool)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

infListPractice :: Integer -> [Integer]
-- infListPractice n = n : infListPractice (fun n)
--         where fun n = case n `mod` 3 of
--                 0 -> n `div` 3
--                 r -> 2 * n + r

infListPractice = iterate fun
        where fun n = case n `mod` 3 of
                0 -> n `div` 3
                r -> 2 * n + r

-- takeTo は foldr と unfoldr の両方で書ける。(16-6)
takeTo :: (a -> Bool) -> [a] -> [a]
-- takeTo p = foldr (\x -> (x :) . bool id (const []) (p x)) []
takeTo p = unfoldr $ \s -> case s of
        [] -> Nothing
        x : xs  | p x -> Just (x, [])
                | otherwise -> Just (x, xs)
