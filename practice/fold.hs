import Funpaala

len :: a -> Integer -> Integer
len = const (1 +)

mySum, myProduct :: [Integer] -> Integer
mySum = myFldr (+) 0
myProduct = myFldr (*) 1

myLength = myFldr len 0

myFldr :: (a -> b -> b) -> b -> [a] -> b
myFldr _ v [] = v
myFldr op v (x : xs) = x `op` myFldr op v xs

sumIter :: Integer -> [Integer] -> Integer
sumIter s [] = s
sumIter s (x : xs) = sumIter (s + x) xs

mySumL :: [Integer] -> Integer
mySumL = sumIter 0

myFldl :: (a -> b -> a) -> a -> [b] -> a
myFldl _ s [] = s
myFldl op s (x : xs) = myFldl op (s `op` x) xs

myFldl' :: (a -> b -> a) -> a -> [b] -> a
myFldl' _ s [] = s
myFldl' op s (x : xs) = s `seq` myFldl' op (s `op` x) xs
