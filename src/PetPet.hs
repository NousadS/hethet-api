{-# LANGUAGE OverloadedStrings #-}

-- Не совсем работающая версия PetPet либы

module PetPet (
    make
) where

import Codec.Picture
import Codec.Picture.Types (promoteImage)

make :: FilePath -> FilePath -> IO ()
make source dest = do
    backgroundImage <- readImage source
    foregroundImage <- readImage "img/pet0.gif"

    case backgroundImage of
        Left err -> do
            putStrLn $ "Cannot load background image: " ++ err

        Right (ImageRGBA8 backgroundImage) -> do
            case foregroundImage of
                Left err -> do
                    putStrLn $ "Cannot load foreground image: " ++ err

                Right (ImageRGBA8 foregroundImage) -> do
                    let resultImage = overlayImage backgroundImage backgroundImage
                    let dynamicImage = ImageRGBA8 resultImage
                    savePngImage dest dynamicImage
                    putStrLn "Image saved successfully!"

                _ -> putStrLn "Foreground image is not in RGBA format"
        _ -> putStrLn "Background image is not in RGBA format"

overlayImage :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
overlayImage background foreground = generateImage blend w h where
    w = min (imageWidth background) (imageWidth foreground)
    h = min (imageHeight background) (imageHeight foreground)
    blend x y = mixWithAlpha (pixelAt background x y) (pixelAt foreground x y)

    mixWithAlpha :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
    mixWithAlpha (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
        let alpha = fromIntegral a2 / 255.0
            invAlpha = 1.0 - alpha
            r = round $ fromIntegral r1 * invAlpha + fromIntegral r2 * alpha
            g = round $ fromIntegral g1 * invAlpha + fromIntegral g2 * alpha
            b = round $ fromIntegral b1 * invAlpha + fromIntegral b2 * alpha
        in PixelRGBA8 r g b 255
