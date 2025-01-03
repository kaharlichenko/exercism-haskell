module Darts (score) where

import Data.Foldable (find)

data Target = Target
  { radius :: Float
  , points :: Int
  }

score :: Float -> Float -> Int
score x y = maybe 0 points $ find hitTarget targets
  where
    distanceToCenter = sqrt (x * x + y * y)
    hitTarget target = distanceToCenter <= radius target
    targets = [ Target {radius =  1.0, points = 10}
              , Target {radius =  5.0, points =  5}
              , Target {radius = 10.0, points =  1}
              ]
