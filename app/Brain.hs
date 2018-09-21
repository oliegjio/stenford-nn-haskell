module Brain where

import Data.Functor
import Control.Monad
import Data.List
import Gauss

newBrain :: [Int] -> IO ([([Float], [[Float]])])
newBrain s@(_:ts) = zip (flip replicate 1 <$> ts) <$>
    zipWithM (\ m n -> replicateM n $ replicateM m $ gauss 0.01) s ts

zLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
zLayer as (bs, ws) = zipWith (+) bs $ sum . zipWith (*) as <$> ws

relu = max 0

feed :: [Float] -> [([Float], [[Float]])] -> [Float]
feed = foldl' (((relu <$>) . ) . zLayer)

dCost a y | y == 1 && a >= y = 0
          | otherwise = a - y

revas :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
revas xv = foldl' (\ (avs@(av:_), zs) (bs, wms) -> let
    zs' = zLayer av (bs, wms) in (((relu <$> zs'):avs), (zs':zs))) ([xv], [])