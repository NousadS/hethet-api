{-# LANGUAGE OverloadedStrings #-}

-- Ебанный GPT чтото написал

module PetPet ( 
    make
) where

import System.IO
import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types
import Control.Monad (forM_)
import Data.ByteString (writeFile)
import Data.Maybe (fromMaybe)

-- Function to scale images
scaleImage :: (Int, Int) -> DynamicImage -> DynamicImage
scaleImage (width, height) img = resizeImage width height img

-- Function to resize a DynamicImage to specified dimensions
resizeImage :: Int -> Int -> DynamicImage -> DynamicImage
resizeImage width height (ImageY8 img) = ImageY8 (resizeImageToImage width height img)
resizeImage width height (ImageYF img) = ImageYF (resizeImageToImage width height img)
resizeImage width height (ImageRGBA8 img) = ImageRGBA8 (resizeImageToImage width height img)
resizeImage _ _ img = img  -- Handle unsupported formats as needed

-- Create a GIF image
make :: FilePath -> FilePath -> Maybe Int -> Maybe (Int, Int) -> Maybe Int -> IO ()
make source dest framesMaybe resolutionMaybe delayMaybe = do
    let frames = fromMaybe 10 framesMaybe
        resolution = fromMaybe (128, 128) resolutionMaybe
        delay = fromMaybe 20 delayMaybe

    imageResult <- readImage source

    case imageResult of
        Left err -> putStrLn $ "Error reading image: " ++ err
        Right img -> do
            let resizedImage = scaleImage resolution img
            let gifImages = replicate frames resizedImage  -- Create frames
            let gifData = GifImage (frameDelay delay) gifImages  -- Ensure frameDelay is imported correctly
            let gif = Gif [gifData]

            writeGifFile dest gif  -- Ensure writeGifFile is imported
            putStrLn $ "GIF created at " ++ dest
