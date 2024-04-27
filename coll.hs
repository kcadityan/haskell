import Data.List (intersect)
-- import Data.List.Index (zipWithIndex)
import Data.List (sortBy)
import Data.Function (on)

isPrime :: (Integral a) => a -> Bool
isPrime n 
  | n <= 1 = False  -- 1 and negatives are not prime
  | n == 2 = True   -- 2 is a prime number
  | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- This helper function calculates the square root for efficiency
isqrt :: Integral a => a -> a
isqrt n = floor (sqrt (fromIntegral n))

coll :: Integer -> Integer
coll x
    | even x    = x `div` 2   
    | otherwise = 3 * x + 1 

append :: Integer -> [Integer]
append n 
    | n == 1 = 1: []
    | otherwise = n : append (coll n)

inter :: Integer -> Integer -> [Integer]
inter a b = (append a) `intersect` (append b)
  
findPrimeColl :: Integer -> [Integer]
findPrimeColl n = filter isPrime (append n)


getter :: Collatz -> Int
getter (Collatz _ xs) = length xs  -- Extract length directly

data Collatz = Collatz Integer [Integer]

sortCollatz :: [Collatz] -> [Collatz]
sortCollatz = sortBy (compare `on` getter)

main :: IO ()
main = do
  let data2 = zip [1..] (map findPrimeColl [1..10000] )
    --  [ (1, [])
    --          , (2, [2])
    --          , (3, [3,5,2])
    --          , (4, [2])
    --          , (5, [5,2])
    --          , (6, [3,5,2])
    --          , (7, [7,11,17,13,5,2])
    --          , (8, [2])
    --          , (9, [7,11,17,13,5,2])
    --          , (10,[5,2])
    --          ]
  let printCollatz (Collatz n xs) = putStrLn ("Number: " ++ show n ++ ", Prime List: " ++ show xs)
  let sorted = sortCollatz $ map (\(n, xs) -> Collatz n xs) data2
  mapM_ printCollatz sorted


-- main :: IO()
-- main = do
--     print (zip [1..] (map findPrimeColl [1..10] ))
    -- print (zip [0..] [0..10])



    
