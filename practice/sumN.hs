sumN :: Integer -> Integer
sumN n = sum [0 .. n]

productN :: Integer -> Integer
productN n = product [1 .. n]

sum3N :: Integer -> Integer
sum3N n = sum $ map (* 3) [1 .. n]

productOdds :: Integer -> Integer
productOdds n = product $ filter odd [1 .. 2 * n + 1]

sum3N5 :: Integer -> Integer
sum3N5 n = sum . map (*3) $ (filter ((/= 0) . (`mod` 5))) [1 .. n]

productOdd3 :: Integer -> Integer
productOdd3 n = product . map (*3) $ filter odd [1 .. n]

