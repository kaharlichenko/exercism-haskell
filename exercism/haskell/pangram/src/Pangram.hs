module Pangram (isPangram) where
import           Data.Char (isAsciiLower, toLower)
import           Data.Set  (empty, insert, size)

isPangram :: String -> Bool
isPangram text = loop empty $ filter isAsciiLower $ map toLower text
  where
    loop seen _ | size seen == 26 = True
    loop _    []                  = False
    loop seen (c:cs)              = loop (insert c seen) cs
