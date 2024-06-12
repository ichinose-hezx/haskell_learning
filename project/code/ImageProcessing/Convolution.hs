module Convolution where

import Codec.Picture
import Helpers

-- 卷积核的类型
type Kernel = [[Double]]

-- 定义高斯模糊卷积核
gaussianKernel :: Kernel
gaussianKernel = [[1/16, 2/16, 1/16],
                  [2/16, 4/16, 2/16],
                  [1/16, 2/16, 1/16]]

-- 对图像的一个像素应用卷积核
applyKernel :: (PixelRGB8 -> Pixel8) -> Kernel -> Image PixelRGB8 -> Int -> Int -> Double
applyKernel component kernel img x y = sum values
  where
    getPixelValue :: Int -> Int -> Double
    getPixelValue i j
        | i < 0 || i >= imageWidth img || j < 0 || j >= imageHeight img = 0
        | otherwise = fromIntegral $ component (pixelAt img i j)
    values = [kernelValue * getPixelValue (x + i) (y + j) | (i, row) <- zip [-1..] kernel, (j, kernelValue) <- zip [-1..] row]


-- 对图像进行卷积
--convolve :: Kernel -> ImageProcessing (Image PixelRGB8) -> ImageProcessing (Image PixelRGB8)
--convolve :: Kernel -> Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
convolve :: Kernel -> Image PixelRGB8 -> Image PixelRGB8
convolve kernel img = convolve' kernel img

convolve' :: Kernel -> Image PixelRGB8 -> Image PixelRGB8

convolve' kernel img = generateImage (\x y -> convolvePixel x y) (imageWidth img) (imageHeight img)
  where
    convolvePixel x y = PixelRGB8 (clamp r) (clamp g) (clamp b)
      where
        r = round $ applyKernel (\(PixelRGB8 r _ _) -> r) kernel img x y
        g = round $ applyKernel (\(PixelRGB8 _ g _) -> g) kernel img x y
        b = round $ applyKernel (\(PixelRGB8 _ _ b) -> b) kernel img x y
