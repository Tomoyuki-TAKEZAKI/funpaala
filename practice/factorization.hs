import Data.List (unfoldr)

popFactor :: Integer -> Maybe (Integer, Integer)
popFactor n | n < 2 = Nothing
popFactor n = Just (f, n `div` f)
        where f = head $ filter ((== 0) . (n `mod`)) [2 ..]

factorization :: Integer -> [Integer]
-- factorization n = case popFactor n of
--         Nothing -> []
--         Just (f, n') -> f : factorization n'

factorization = unfoldr popFactor

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = case f s of
        Nothing -> []
        Just (x, a') -> x : myUnfoldr f a'

dict :: [(Integer, (Char, Integer))]
dict = [
        (5, ('k', 11)),
        (7, ('l', 3)),
        (0, ('H', 9)),
        (17, ('s', 5)),
        (9, ('a', 17)),
        (11, ('e', 7)),
        (3, ('l', 20))
        ]

integerToList :: Integer -> [Integer]
-- integerToList n = case dm10 n of
--         Nothing -> []
--         Just (r, q) -> r : integerToList q
integerToList = unfoldr dm10

dm10 :: Integer -> Maybe (Integer, Integer)
dm10 n = case n `divMod` 10 of
        (0, 0) -> Nothing
        (q, r) -> Just (r, q)

listToInteger :: [Integer] -> Integer
-- listToInteger [] = 0
-- listToInteger (n : ns) = n `op` (listToInteger ns)
--         where op n x = n + 10 * x
listToInteger = foldr (\n x -> n + 10 * x) 0
