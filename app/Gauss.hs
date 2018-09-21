module Gauss where

import System.Random

gauss :: Float -> IO Float
gauss stdev = do
    x1 <- randomIO
    x2 <- randomIO
    return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)