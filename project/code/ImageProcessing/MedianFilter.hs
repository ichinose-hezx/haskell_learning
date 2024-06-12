module MedianFilter (denoise) where

import Codec.Picture
import Data.List (sort)
import Helpers

-- 定义辅助函数用于提取像素的红、绿和蓝色分量
component0 :: PixelRGB8 -> Pixel8
component0 (PixelRGB8 r _ _) = r

component1 :: PixelRGB8 -> Pixel8
component1 (PixelRGB8 _ g _) = g

component2 :: PixelRGB8 -> Pixel8
component2 (PixelRGB8 _ _ b) = b

--denoise :: ImageProcessing (Image PixelRGB8) -> ImageProcessing (Image PixelRGB8)
--denoise :: Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
denoise :: Image PixelRGB8 -> Image PixelRGB8
denoise img = denoise' img


denoise' :: Image PixelRGB8 -> Image PixelRGB8
denoise' img = generateImage medianFilter (imageWidth img) (imageHeight img)
  where
    kernelRadius = 1
    medianFilter x y = PixelRGB8 (median (getValues component0 x y))
                                 (median (getValues component1 x y))
                                 (median (getValues component2 x y))
    getValues component i j = sort [component (pixelAtSafe img (i + dx - kernelRadius) (j + dy - kernelRadius))
                                    | dy <- [0..2*kernelRadius], dx <- [0..2*kernelRadius]]
    median values = values !! (length values `div` 2)
