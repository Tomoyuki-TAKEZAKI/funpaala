introduction :: (String, Integer) -> String
introduction (n, a) = "My name is " ++ n ++ ". I'm " ++ show a ++ " years old."

introductionT :: Integer -> String
introductionT = curry introduction "Takezaki"

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

age :: (Integer, Bool) -> String
age (n, isPublic)
        | isPublic = show n
        | otherwise = "secret"

age39 :: Bool -> String
age39 = curry age 39

isTriangle :: (Integer, Integer, Integer) -> Bool
isTriangle (a, b, c) =
        a > 0 && b > 0 && c > 0 &&
        a + b > c && b + c > a && c + a > b

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)
