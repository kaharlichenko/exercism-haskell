module ReverseString (reverseString) where

reverseString :: String -> String
reverseString = inner []
  where
    inner acc [] = acc
    inner acc (x:xs) = inner (x:acc) xs
