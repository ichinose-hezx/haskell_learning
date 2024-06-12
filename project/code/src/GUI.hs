{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GUI where

import Graphics.UI.Gtk hiding (Image)
import qualified Graphics.UI.Gtk as Gtk (Image)
import Codec.Picture
import Convolution
import Brightness
import Contrast
import Gamma
import MedianFilter
import Helpers

-- 加载图像
loadImage :: FilePath -> IO (Either String DynamicImage)
loadImage path = readImage path

-- 保存图像
saveImage :: FilePath -> DynamicImage -> IO ()
saveImage path (ImageRGB8 img) = savePngImage path (ImageRGB8 img)
saveImage _ _ = putStrLn "Unsupported image format"

-- 应用处理函数
applyProcessing :: Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Double -> Double -> Image PixelRGB8 -> Image PixelRGB8
applyProcessing useConvolution useBrightness useContrast useGamma useDenoise brightnessValue contrastValue gammaValue img =
    let img1 = if useConvolution then convolve gaussianKernel img else img
        img2 = if useBrightness then adjustBrightness brightnessValue img1 else img1
        img3 = if useContrast then adjustContrast contrastValue img2 else img2
        img4 = if useGamma then adjustGamma gammaValue img3 else img3
        img5 = if useDenoise then denoise img4 else img4
    in img5

-- 设置按钮点击事件
onApplyButtonClick :: FilePath -> Gtk.Image -> CheckButton -> CheckButton -> CheckButton -> CheckButton -> CheckButton -> Entry -> Entry -> Entry -> IO ()
onApplyButtonClick inputPath imageWidget convChkBtn brightChkBtn contrastChkBtn gammaChkBtn denoiseChkBtn brightEntry contrastEntry gammaEntry = do
    useConvolution <- toggleButtonGetActive convChkBtn
    useBrightness <- toggleButtonGetActive brightChkBtn
    useContrast <- toggleButtonGetActive contrastChkBtn
    useGamma <- toggleButtonGetActive gammaChkBtn
    useDenoise <- toggleButtonGetActive denoiseChkBtn

    brightnessValue <- read <$> entryGetText brightEntry :: IO Int
    contrastValue <- read <$> entryGetText contrastEntry :: IO Double
    gammaValue <- read <$> entryGetText gammaEntry :: IO Double

    imgResult <- loadImage inputPath
    case imgResult of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right (ImageRGB8 img) -> do
            let processedImg = applyProcessing useConvolution useBrightness useContrast useGamma useDenoise brightnessValue contrastValue gammaValue img
            saveImage "output/output_image.png" (ImageRGB8 processedImg)
            imageSetFromFile imageWidget "output/output_image.png"

-- 主函数，初始化 GUI
runGUI :: IO ()
runGUI = do
    -- 初始化 GTK
    initGUI

    -- 创建窗口
    window <- windowNew
    set window [windowTitle := ("Image Processor" :: String), containerBorderWidth := 10, windowDefaultWidth := 800, windowDefaultHeight := 600]

    -- 创建布局
    vbox <- vBoxNew False 10
    containerAdd window vbox

    -- 创建图像控件
    imageWidget <- imageNew
    boxPackStart vbox imageWidget PackGrow 0

    -- 创建复选框和输入框
    convChkBtn <- checkButtonNewWithLabel ("Use Convolution" :: String)
    boxPackStart vbox convChkBtn PackNatural 0

    brightChkBtn <- checkButtonNewWithLabel ("Use Brightness(-255,255)" :: String)
    boxPackStart vbox brightChkBtn PackNatural 0
    brightEntry <- entryNew
    boxPackStart vbox brightEntry PackNatural 0

    contrastChkBtn <- checkButtonNewWithLabel ("Use Contrast" :: String)
    boxPackStart vbox contrastChkBtn PackNatural 0
    contrastEntry <- entryNew
    boxPackStart vbox contrastEntry PackNatural 0

    gammaChkBtn <- checkButtonNewWithLabel ("Use Gamma" :: String)
    boxPackStart vbox gammaChkBtn PackNatural 0
    gammaEntry <- entryNew
    boxPackStart vbox gammaEntry PackNatural 0

    denoiseChkBtn <- checkButtonNewWithLabel ("Use Denoise" :: String)
    boxPackStart vbox denoiseChkBtn PackNatural 0

    -- 创建按钮
    button <- buttonNewWithLabel ("Apply Processing" :: String)
    boxPackStart vbox button PackNatural 0

    -- 设置按钮点击事件
    on button buttonActivated $ onApplyButtonClick "input/kodim15.png" imageWidget convChkBtn brightChkBtn contrastChkBtn gammaChkBtn denoiseChkBtn brightEntry contrastEntry gammaEntry

    -- 显示所有控件
    widgetShowAll window

    -- 连接关闭事件
    on window objectDestroy mainQuit

    -- 开始主循环
    mainGUI