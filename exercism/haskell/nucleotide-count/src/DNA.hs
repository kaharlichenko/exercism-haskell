module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Foldable (foldlM)
import Data.Map (Map, alter, empty)
import Text.Read (readEither)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldlM process empty
  where
    process counts c = do
      nucleotide <- transcribe c
      return $ alter increment nucleotide counts
    increment = Just . maybe 1 (+ 1)
    transcribe c = readEither [c]
