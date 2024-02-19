import Data.Char as C

safeRecip :: Double -> Maybe Double
safeRecip = \x -> case x of
  0 -> Nothing
  _ -> Just $ 1 / x


checkAnswer :: Char -> Maybe Bool
checkAnswer c = case C.toLower c of
  'y' -> Just True
  'n' -> Just False
  _ -> Nothing

-- diffRecip :: Double -> Double -> Maybe Double
-- diffRecip x y = case x - y of
--   0 -> Nothing
--   d | d > 0 -> Just $ recip d
--     | otherwise -> Just $ recip (-d)

diffRecip :: Double -> Double -> Maybe Double
diffRecip x y = case x - y of
  0 -> Nothing
  d -> Just . recip . abs $ d

format :: String -> [(String, Int)] -> String
format k d = case lookup k d of
  Just n -> replicate (5 - length s) ' ' ++ s
    where s = show n
  Nothing -> "NO VALUE"

oneToFive :: Integer -> Integer
oneToFive = \n -> case n `mod` 5 of
  0 -> 5
  r -> r