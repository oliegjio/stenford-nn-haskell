module Render (renderRandomImage) where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import System.Random

render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

renderRandomImage :: String -> String -> IO ()
renderRandomImage imagesPath labelsPath = do
    s <- decompress <$> BS.readFile imagesPath
    l <- decompress <$> BS.readFile labelsPath 
    n <- (`mod` 60000) <$> randomIO
    putStr . unlines $ [(render . BS.index s . (n * 28^2 + 16 + r * 28 +)) <$> [0..27] | r <- [0..27]]
    print $ BS.index l (n + 8)