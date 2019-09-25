module SimpleFunc (
  qs,
  fact,
  readAInt,
  readAInteger,
  readADouble,
  filter_c,
  Predicate
) where 

qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = (qs l) ++ [x] ++ (qs g)
    where
        l = filter (< x) xs
        g = filter (>= x) xs
        
fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

readAInt :: IO Int
readAInt = readLn

readAInteger :: IO Integer
readAInteger = readLn

readADouble :: IO Double
readADouble = readLn

type Predicate a = a -> Bool

filter_c :: Predicate a -> [a] -> [a]
filter_c _ [] = []
filter_c p (x:xs)
          | p x = x : filter_c p xs
          | otherwise = filter_c p xs
