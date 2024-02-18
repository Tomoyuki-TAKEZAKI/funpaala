dist0 :: (Double, Double) -> Double
dist0 (x, y) = sqrt $ x ^ 2 +  y ^ 2

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = dist0 ((x1 - x2), (y1 - y2))