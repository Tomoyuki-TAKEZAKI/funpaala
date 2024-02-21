import Text.Read (readMaybe)
import Funpaala
import Data.Bool (bool)

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

rpolish :: [String] -> Maybe [Integer]
-- rpolish = rpolishIter $ Just []
rpolish = fldl' rpolish1 $ Just []

rpolishIter :: Maybe [Integer] -> [String] -> Maybe [Integer]
rpolishIter mns [] = mns
rpolishIter (Just ns) (s : ss) = case lookup s operators of
        Just o -> case ns of
                y : x : ns' -> rpolishIter (Just $  x `o` y : ns') ss
                _ -> Nothing
        Nothing -> rpolishIter (fmap (: ns) $ readMaybe s) ss
rpolishIter Nothing _ = Nothing

rpolish1 :: Maybe [Integer] -> String -> Maybe [Integer]
rpolish1 (Just ns) s = case lookup s operators of
        Just o -> case ns of
                y : x : ns' -> Just $ x `o` y : ns'
                _ -> Nothing
        Nothing -> fmap (: ns) $ readMaybe s
rpolish1 Nothing _ = Nothing

boolToOp :: Bool -> Integer -> Integer -> Integer
boolToOp = bool (+) (*)

-- boolFoldl :: [(Bool, Integer)] -> Integer
-- boolFoldl = boolFoldlIter 0

-- boolFoldlIter :: Integer -> [(Bool, Integer)] -> Integer
-- boolFoldlIter acc [] = acc
-- boolFoldlIter acc ((b, n) : ns) = boolFoldlIter (acc `op` n) ns
--         where op = boolToOp b

boolFoldl :: [(Bool, Integer)] -> Integer
boolFoldl = fldl' boolFoldl1 0

boolFoldl1 :: Integer -> (Bool, Integer) -> Integer
boolFoldl1 x (b, y) = x `op` y
        where op = boolToOp b
