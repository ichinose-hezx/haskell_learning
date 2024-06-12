
module Main where

import Codec.Picture
import Codec.Picture.Types
import qualified Data.Text.IO as TIO
import Data.Text (unpack)
import Control.Monad.Reader
import Control.Monad.Writer
import Helpers

main :: IO ()
main = do
    imgResult <- readImage "input_image.png"
    case imgResult of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right (ImageRGB8 img) -> do
            let functions = [ ("convolve", "0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0")
                            , ("adjustBrightness", "50")
                            , ("adjustContrast", "1.5")
                            , ("adjustGamma", "0.5")
                            , ("denoise", "") ]
            let (result, logs) = runImageProcessing (processImage img functions)
            TIO.writeFile "output_image.png" logs
            savePngImage "output_image.png" (ImageRGB8 result)

applyFunction :: Image PixelRGB8 -> (String, String) -> ImageProcessing (Image PixelRGB8)
applyFunction img ("convolve", params) = do
    let kernel = map read $ words params :: [Double]
    convolve kernel img
applyFunction img ("adjustBrightness", params) = do
    let delta = read params :: Int
    adjustBrightness (fromIntegral delta) img
applyFunction img ("adjustContrast", params) = do
    let factor = read params :: Double
    adjustContrast factor img
applyFunction img ("adjustGamma", params) = do
    let gamma = read params :: Double
    adjustGamma gamma img
applyFunction img ("denoise", _) = denoise img
applyFunction img _ = return img

processImage :: Image PixelRGB8 -> [(String, String)] -> ImageProcessing (Image PixelRGB8)
processImage img [] = return img
processImage img ((func, params):funcs) = do
    img' <- applyFunction img (func, params)
    processImage img' funcs


module Helpers where

import Codec.Picture
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer as Writer

import Control.Monad.Writer.Class

type ImageProcessing a = ReaderT String (Writer.Writer String) a

runImageProcessing :: String -> ImageProcessing a -> (a, String)
runImageProcessing config action = Writer.runWriter (runReaderT action config)

logMessage :: String -> ImageProcessing ()
logMessage msg =  ReaderT (\_ -> Writer.tell (msg ++ "\n"))

clamp :: Int -> Int -> Int -> Int
clamp val minVal maxVal = max minVal (min maxVal val)

pixelAtSafe :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
pixelAtSafe img x y
    | x < 0 || x >= imageWidth img || y < 0 || y >= imageHeight img = PixelRGB8 0 0 0
    | otherwise = pixelAt img x y

module Brightness where

import Codec.Picture
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Control.Monad.Writer.Class
import Helpers (ImageProcessing, logMessage, clamp)

-- 调整亮度
adjustBrightness :: Int -> Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
adjustBrightness delta img = do
    logMessage "Starting brightness adjustment"
    let result = adjustBrightness' delta img
    logMessage "Brightness adjustment completed successfully"
    return result

-- 实际调整亮度的函数
adjustBrightness' :: Int -> Image PixelRGB8 -> Image PixelRGB8
adjustBrightness' delta img = pixelMap adjustPixel img
  where
    adjustPixel (PixelRGB8 r g b) = PixelRGB8 (clamp r') (clamp g') (clamp b')
      where
        r' = fromIntegral r + delta
        g' = fromIntegral g + delta
        b' = fromIntegral b + delta
        clamp val = fromIntegral $ min 255 (max 0 val)
module Contrast where

import Codec.Picture
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Control.Monad.Writer.Class
import Helpers (ImageProcessing, logMessage, clamp)

-- 调整对比度
adjustContrast :: Double -> Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
adjustContrast factor img = do
    logMessage "Starting contrast adjustment"
    let result = adjustContrast' factor img
    logMessage "Contrast adjustment completed successfully"
    return result

-- 实际调整对比度的函数
adjustContrast' :: Double -> Image PixelRGB8 -> Image PixelRGB8
adjustContrast' factor img = pixelMap adjustPixel img
  where
    adjustPixel (PixelRGB8 r g b) = PixelRGB8 (clamp r') (clamp g') (clamp b')
      where
        r' = round $ fromIntegral r * factor
        g' = round $ fromIntegral g * factor
        b' = round $ fromIntegral b * factor
        clamp val = fromIntegral $ min 255 (max 0 val)
module Convolution where

import Codec.Picture
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Control.Monad.Writer.Class
import Helpers (ImageProcessing, logMessage, clamp)

-- 卷积核的类型
type Kernel = [[Double]]

-- 对图像的一个像素应用卷积核
applyKernel :: Kernel -> Image PixelRGB8 -> Int -> Int -> Double
applyKernel kernel img x y = sum values
  where
    getPixelValue :: Int -> Int -> Double
    getPixelValue i j
        | i < 0 || i >= imageWidth img || j < 0 || j >= imageHeight img = 0
        | otherwise = fromIntegral $ component (pixelAt img i j)
    component (PixelRGB8 r _ _) = fromIntegral r -- 只取红色通道作为像素值

    values = [kernelValue * getPixelValue (x + i) (y + j) | (i, row) <- zip [-1..] kernel, (j, kernelValue) <- zip [-1..] row]

-- 对图像进行卷积
convolve :: Kernel -> Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
convolve kernel img = do
    logMessage "Starting convolution"
    let result = convolve' kernel img
    logMessage "Convolution completed successfully"
    return result

convolve' :: Kernel -> Image PixelRGB8 -> Image PixelRGB8
convolve' kernel img = generateImage (\x y -> convolvePixel x y) (imageWidth img) (imageHeight img)
  where
    convolvePixel x y = PixelRGB8 (clamp r) (clamp g) (clamp b)
      where
        r = round $ applyKernel kernel img x y
        g = round $ applyKernel kernel img x y
        b = round $ applyKernel kernel img x y
        clamp val = fromIntegral $ min 255 (max 0 val)
module Gamma where

import Codec.Picture
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer as Writer

import Control.Monad.Writer.Class
import Helpers (ImageProcessing, logMessage, clamp)

-- 调整像素的 gamma
adjustPixel :: Double -> PixelRGB8 -> PixelRGB8
adjustPixel gamma (PixelRGB8 r g b) = PixelRGB8 (clamp r') (clamp g') (clamp b')
  where
    clamp val = fromIntegral $ min 255 (max 0 (round (fromIntegral val ** gamma)))
    r' = clamp r
    g' = clamp g
    b' = clamp b

-- 对图像进行 gamma 调整
adjustGamma :: Double -> Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
adjustGamma gamma img = do
    logMessage "Starting gamma adjustment"
    let adjustedImg = pixelMap (adjustPixel gamma) img
    logMessage "Gamma adjustment completed successfully"
    return adjustedImg
module MedianFilter (denoise) where

import Codec.Picture
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.List (sort)
import Helpers

-- 定义辅助函数用于提取像素的红、绿和蓝色分量
component0 :: PixelRGB8 -> Pixel8
component0 (PixelRGB8 r _ _) = r

component1 :: PixelRGB8 -> Pixel8
component1 (PixelRGB8 _ g _) = g

component2 :: PixelRGB8 -> Pixel8
component2 (PixelRGB8 _ _ b) = b

denoise :: Image PixelRGB8 -> ImageProcessing (Image PixelRGB8)
denoise img = do
    logMessage "Starting denoising"
    let result = denoise' img
    logMessage "Denoising completed successfully"
    return result

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
