module Brightness where

import Codec.Picture

import Helpers 

-- 调整亮度
--adjustBrightness :: Int -> ImageProcessing (Image PixelRGB8) -> ImageProcessing (Image PixelRGB8)
--adjustBrightness :: Int -> Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
adjustBrightness :: Int -> Image PixelRGB8 -> Image PixelRGB8
adjustBrightness delta img = adjustBrightness' delta img

-- 实际调整亮度的函数
adjustBrightness' :: Int -> Image PixelRGB8 -> Image PixelRGB8
adjustBrightness' delta img = pixelMap adjustPixel img
  where
    adjustPixel (PixelRGB8 r g b) = PixelRGB8 (clamp r') (clamp g') (clamp b')
      where
        r' = fromIntegral r + delta
        g' = fromIntegral g + delta
        b' = fromIntegral b + delta
