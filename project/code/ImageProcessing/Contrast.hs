module Contrast where

import Codec.Picture
import Helpers 

-- 调整对比度
adjustContrast :: Double -> Image PixelRGB8 -> Image PixelRGB8
--adjustContrast :: Double -> ImageProcessing (Image PixelRGB8)-> ImageProcessing (Image PixelRGB8)
--adjustContrast :: Double -> Image PixelRGB8-> ImageProcessing (Image PixelRGB8)
adjustContrast factor img = adjustContrast' factor img

-- 实际调整对比度的函数
adjustContrast' :: Double -> Image PixelRGB8 -> Image PixelRGB8
adjustContrast' factor img = pixelMap adjustPixel img
  where
    adjustPixel (PixelRGB8 r g b) = PixelRGB8 (clamp r') (clamp g') (clamp b')
      where
        r' = round $ fromIntegral r * factor
        g' = round $ fromIntegral g * factor
        b' = round $ fromIntegral b * factor
