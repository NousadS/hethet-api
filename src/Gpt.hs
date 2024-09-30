{-# LANGUAGE OverloadedStrings #-}

-- ЖПТ чета написал не понятное

import Codec.Picture (readImage, DynamicImage(..), writePng, Image, Pixel8)
import qualified Data.ByteString.Lazy as BL

-- Function to convert a DynamicImage to ByteString
dynamicImageToBytes :: DynamicImage -> BL.ByteString
dynamicImageToBytes (ImageY8 img) = writeImageToBytes img
dynamicImageToBytes (ImageRGB8 img) = writeImageToBytes img
dynamicImageToBytes (ImageRGBA8 img) = writeImageToBytes img
dynamicImageToBytes _ = error "Unsupported image format"

-- Helper function to write Image to Bytes
writeImageToBytes :: Image Pixel8 -> BL.ByteString
writeImageToBytes img = BL.toStrict $ writePng img

writePng :: Image Pixel8 -> BL.ByteString
writePng img = BL.fromStrict $ BL.toStrict $ writePng img

gpt_make :: IO ()
gpt_make = do
    eitherImg <- readImage "img/source.gif"
    case eitherImg of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right dynImg -> do
            let bytes = dynamicImageToBytes dynImg
            BL.writeFile "output_image_bytes.png" bytes
            putStrLn "Image converted to bytes and saved."
