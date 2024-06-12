module Gamma where

import Codec.Picture
import Helpers 

adjustPixel :: Double -> PixelRGB8 -> PixelRGB8
adjustPixel gamma (PixelRGB8 r g b) = PixelRGB8 (clamp r') (clamp g') (clamp b')
  where
    r' = fromIntegral $ round $ (fromIntegral r / 255.0) ** gamma * 255.0
    g' = fromIntegral $ round $ (fromIntegral g / 255.0) ** gamma * 255.0
    b' = fromIntegral $ round $ (fromIntegral b / 255.0) ** gamma * 255.0


-- Function to adjust the gamma of an entire image
adjustGamma' :: Double -> Image PixelRGB8 -> Image PixelRGB8
adjustGamma' gamma img = pixelMap (adjustPixel gamma) img

adjustGamma :: Double -> Image PixelRGB8 -> Image PixelRGB8
adjustGamma gamma img = adjustGamma' gamma img