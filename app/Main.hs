module Main where

import Render

main :: IO ()
main = do
    renderRandomImage "dataset/train-images-idx3-ubyte.gz" "dataset/train-labels-idx1-ubyte.gz"
    return ()
