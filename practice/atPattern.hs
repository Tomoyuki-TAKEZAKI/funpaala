dupHead :: [a] -> [a]
-- dupHead [] = []
-- dupHead (x : xs) = x : x : xs
dupHead xss@(x : _) = x : xss
