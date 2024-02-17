import Data.Char as Ch

myFromMaybe :: a -> Maybe a -> a
myFromMaybe _ (Just x) = x
myFromMaybe d _ = d

myFromMaybe' :: a -> Maybe a -> a
myFromMaybe' a b = maybe a id b

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe _ f (Just x) = f x
myMaybe d _ Nothing = d

myId :: a -> a
myId x = x

myConst :: a -> b -> a
myConst x _ = x

const' :: a -> b -> b
const' _ x = x

-- https://scrapbox.io/point-at-infinity/infixr_%E3%81%A8%E9%96%A2%E6%95%B0%E9%81%A9%E7%94%A8%E6%BC%94%E7%AE%97%E5%AD%90
infixr 0 .$.
(.$.) :: (a -> b) -> a -> b
f .$. x = f x

apply :: a -> (a -> b) -> b
apply x f = f x

(...) :: (b -> c) -> (a -> b) -> a -> c
f ... g = \x -> f (g x)

toLowerOrd :: Char -> Int 
toLowerOrd = Ch.ord . Ch.toLower

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x

flip13 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip13 f x y z = f z y x

myOn :: (b -> b -> c) -> (a -> b) -> a -> a -> c
myOn op f x y = f x `op` f y

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c 
on3 op f x y z = op (f x) (f y) (f z)