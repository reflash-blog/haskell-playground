import SimpleFunc
import Problems

showTuple (x,y) = "(" ++ show x ++ ", " ++ show y ++ ")"

main = do
  putStrLn "n?"
  n <- readAInteger
  
  putStrLn ( "fib: " ++ (show $ fibonacci n))