
--     0 1 1 2 3  5  8 13 21 ...  <- これは fibs
--   + 1 1 2 3 5  8 13 21 34 ...  <- これは tfibs
-- ------------------------------
-- 0 1 1 2 3 5 8 13 21 34 55 ...  <- fibs = 0 : 1 : (fibs と tfibs の項ごとに和をとったもの。zipWith で表現できる)
fibs, tfibs :: [Integer]
fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs

-- Jacobsthal numbers
-- https://oeis.org/A001045
jacobsthals, tjacobsthals :: [Integer]
jacobsthals@(_ : tjacobsthals) = 0 : 1 : zipWith (\j tj -> 2 * j + tj) jacobsthals tjacobsthals
