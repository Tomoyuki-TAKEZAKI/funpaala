import Data.Function as F

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y

tax :: Integer -> Integer -> Integer
tax p t = p + p * t `div` 100

congruent :: Integer -> Integer -> Integer -> Bool
congruent x y z = on (==) (`mod` x) y z 

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
