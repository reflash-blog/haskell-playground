module Problems (
  roots,
  seqA,
  sum'n'count,
  integration,
  fibonacci,
  groupElems
) where 

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = (x_1, x_2)
  where d = sqrt (4*a*c)
        x_n op = -b `op` d / (2*a)
        x_1 = x_n (-)
        x_2 = x_n (+)
        
seqA :: Integer -> Integer
seqA n = helper 1 2 3 n
    where helper acc1 acc2 acc3 n 
                | n == 0     = acc1
                | n == 1     = acc2
                | n == 2     = acc3
                | otherwise  = helper acc2 acc3 (acc3 + acc2 - 2 * acc1) (n-1)
                
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = if x == 0 then (0,1) else helper 0 0 x
  where helper sum count 0 = (sum, count)
        helper sum count x 
          | x < 0     = helper sum count (-x)
          | otherwise = let (c, s) = quotRem x 10
                        in helper (sum+s) (count+1) c


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b 
  | b >= a    = integrationStep 0 a (abs(b-a)/n) n
  | otherwise = - (integrationStep 0 b (abs(a-b)/n) n)
  where 
    integrationStep s l step 0 = s
    integrationStep s l step iter = 
      integrationStep (s + trapezium l (l+step)) (l+step) step (iter-1)
    trapezium xi xj = (f(xi) + f (xj)) / 2 * (xj - xi)
    n = 1000

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n 
    | n > 1 = fibonacci (n-1) + fibonacci (n-2)
    | n < 0 = (-1) ^ (-n+1) * fibonacci (-n)
    
fibonacci2 :: Integer -> Integer
fibonacci2 n 
    | n > 1 = helper 0 1 n
    | n < 0 = (-1) ^ (-n+1) * fibonacci2 (-n)
    where 
        helper acc1 acc2 0 = acc1
        helper acc1 acc2 n = helper acc2 (acc1+acc2) (n-1)
        
groupE :: Eq a => a -> [a] -> [a] -> ([a], [a])
groupE x acc [] = (acc, [])
groupE x acc (h:[]) = if x == h then ((h:acc),[]) else ((acc),[h])
groupE x acc (h:t) = if x == h then groupE x (h:acc) t else (acc, (h:t))

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:[]) = [[x]]
groupElems (x:xs) = let (acc,nxs) = groupE x [] xs
                    in [x:acc] ++ groupElems nxs


cc :: [a] -> Int -> [[a]]
cc xs 0 = [xs] 
cc (x:xs) n = (x:xs) : cc (xs++[x]) (n - 1)

perms :: [a] -> [[a]]
perms []   = []
perms [x]  = [[x]]
perms nums = concatMap pp (cc nums (length nums - 1))
    where 
        pp :: [a] -> [[a]]
        pp (x:xs) = map (x:) (perms xs)
        

data Odd = Odd Integer deriving (Eq,Show)

instance Enum Odd where
  succ (Odd x) = Odd (x+2)
  pred (Odd x) = Odd (x-2)
  toEnum x = Odd (toInteger x)
  fromEnum (Odd x) = fromInteger x
  enumFrom xo@(Odd x) = enumFromThen xo (Odd (x+2))
  enumFromTo ox@(Odd x) oy@(Odd y) = if x > y then [] 
                               else enumFromThenTo ox (Odd (x+2)) oy
  enumFromThen (Odd x) (Odd y) = map Odd [x,y ..]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = 
                    map Odd [x,x+(y-x) .. z]
                    
meanList :: [Double] -> Double
meanList xs = (/len) $ foldr (+) 0.0 xs
  where len = fromIntegral (length xs)

evenOnly :: [a] -> [a]
evenOnly = fst . foldl (\(acc,c) x -> if c then (acc++[x], False) else (acc, True) ) ([], False)


evenOnly' :: [a] -> [a]
evenOnly' = foldr (\(c,x) acc ->if c then x:acc else acc ) [] . zip (cycle [False, True])

revRange :: (Char,Char) -> [Char]
revRange (x,y) = unfoldr g y
  where g a = if a >= x then Just (a, pred a) else Nothing
  
change :: (Ord a, Num a) => a -> [[a]]
change n = helper coins [] 
    where helper [] _  = []
          helper xa@(x:xs) acc 
            | (sum acc + x) == n = 
                  cc (x:acc) (length acc) ++ helper xs acc
            | (sum acc + x) < n = 
                  helper xa (x:acc) ++ helper xs acc
            | (sum acc + x) > n = 
                  helper xs acc
                  
-- if divisible then [x] * n/x
-- for each coin call change for change (n-x)

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand exp = if expanded == exp then exp else expand expanded
  where 
  expand' :: Expr -> Expr
  expand' ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
  expand' (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
  expand' (e1 :+: e2) = expand e1 :+: expand e2
  expand' (e1 :*: e2) = expand e1 :*: expand e2
  expand' e = e
  expanded = expand' exp

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where
    empty = ListMap []
    lookup k m = L.lookup k (getListMap m)
    insert k v m = case lookup k m of
        Just x -> ListMap $ (k,v) : (getListMap $ delete k m)
        Nothing -> ListMap $ (k,v) : (getListMap m)
    delete k m = case lookup k m of
                 Nothing -> m
                 Just v -> ListMap $ L.deleteBy (\(x,y) (k,v) -> x == k) (k,v) (getListMap m)
                 
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
   deriving (Eq, Show)

import Data.Char (isDigit)


asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken n
    | and (map isDigit n) = Just $ Number $ read n
    | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map asToken . words