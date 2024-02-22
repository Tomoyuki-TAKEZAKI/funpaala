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
