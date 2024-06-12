module Main where

import GUI (runGUI)

main :: IO ()
main = runGUI

{-
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Picture
--import Codec.Picture.Types
--import qualified Data.Text.IO as TIO
--import Data.Text (unpack)
import Convolution
import Brightness
import Contrast
import Gamma
import MedianFilter
import Helpers

processImage :: Image PixelRGB8 -> Image PixelRGB8
processImage = denoise . adjustGamma 1.1 . adjustContrast 1 . adjustBrightness 0 . convolve gaussianKernel

main :: IO ()
main = do
    imgResult <- readImage "/home/kakehashizx/haskell_learning/project/code/input/kodim15.png"
    case imgResult of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right (ImageRGB8 img) -> do
            let processedImg = processImage img
            savePngImage "/home/kakehashizx/haskell_learning/project/code/output/output_image.png" (ImageRGB8 processedImg)
-}

{-
main :: IO ()
main = do
    imgResult <- readImage "/home/kakehashizx/haskell_learning/project/code/input/kodim15.png"
    case imgResult of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right (ImageRGB8 img) -> do
            let kernel = gaussianKernel -- 生成卷积核
--          let kernel = [[1.0, 1.0, 1.0], [1.0, 1.0, 1.0], [1.0, 1.0, 1.0]] -- 生成卷积核
            let img1 = convolve kernel img -- 第一个处理过程的图像
            let img2 = adjustBrightness 0 img1 -- 第二个处理过程的图像
            let img3 = adjustContrast 1 img2 -- 第三个处理过程的图像
            let img4 = adjustGamma 1 img3 -- 第四个处理过程的图像
            let img5 = denoise img4 -- 第五个处理过程的图像
            -- 保存最终处理过的图像
            savePngImage "/home/kakehashizx/haskell_learning/project/code/output/output_image.png" (ImageRGB8 img1)
-}
