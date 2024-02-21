import Text.Read (readMaybe)
import Data.Bool (bool)
import Funpaala

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

-- polish :: [String] -> Maybe [Integer]
-- polish [] = Just []
-- polish (s : ss) = case lookup s operators of 
--         Just o -> case polish ss of
--                 Just (x : y : ns) -> Just $ (x `o` y) : ns
--                 Nothing -> Nothing
--         Nothing -> case readMaybe s of
--                 Just n -> fmap (n :) $ polish ss
--                 Nothing -> Nothing

polish1 :: String -> Maybe [Integer] -> Maybe [Integer]
polish1 s (Just ns) = case lookup s operators of
        Just o -> case ns of 
                x : y : ns' -> Just $ x `o` y : ns'
                _ -> Nothing
        Nothing -> fmap (: ns) $ readMaybe s
polish1 _ Nothing = Nothing

polish :: [String] -> Maybe [Integer]
polish = fldr polish1 (Just [])

boolToOp :: Bool -> (Integer -> Integer -> Integer)
boolToOp = bool (+) (*)

-- boolFoldr :: [(Bool, Integer)] -> Integer
-- boolFoldr [] = 0
-- boolFoldr ((b, n) : xs) = boolFoldr xs `op` n
--         where op = boolToOp b

boolFoldr :: [(Bool, Integer)] -> Integer
boolFoldr = fldr boolFoldr1 0

boolFoldr1 :: (Bool, Integer) -> Integer -> Integer
boolFoldr1 (b, x) sum = sum `op` x
        where op = boolToOp b