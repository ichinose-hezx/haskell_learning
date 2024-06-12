module Helpers where

import Codec.Picture

--type ImageProcessing a = ReaderT String (Writer.Writer String) a
--runImageProcessing :: ImageProcessing a -> (a, String)
--runImageProcessing action = Writer.runWriter (runReaderT action "")

maxVal :: Int
maxVal = 255
minVal :: Int
minVal = 0

clamp :: Int -> Pixel8
clamp val = fromIntegral $ max minVal (min maxVal (fromIntegral val))


pixelAtSafe :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
pixelAtSafe img x y
    | x < 0 || x >= imageWidth img || y < 0 || y >= imageHeight img = PixelRGB8 0 0 0
    | otherwise = pixelAt img x y
