module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n > 0     = Just $ loop n 0
  | otherwise = Nothing
  where
    loop 1 steps = steps
    loop num steps
      | even num  = loop (num `div` 2) (steps + 1)
      | otherwise = loop (num * 3 + 1) (steps + 1)
