{-# LANGUAGE OverloadedStrings #-}

-- Тоже ЖПТ написал

module Server ( 
    runAPI
) where

import           Control.Monad.Trans.Resource
import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (run)
import Control.Monad (join)
import Data.CaseInsensitive (mk)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- Function to create a text response
make_text_response :: (Response -> b) -> [Char] -> b
make_text_response respond text = respond $ responseLBS status200 [(mk (BS.pack "Content-Type"), BS.pack "text/plain")] (LBS.pack text)

-- Application logic
application :: Application
application request respond = do
    putStrLn $ "New request: " ++ BS.unpack (rawPathInfo request)

    -- Check if the request is for the "/stream" endpoint
    if rawPathInfo request == "/stream"
        then streamingResponse respond
        else handleImageRequest request respond

-- Handle image request with parameters
handleImageRequest :: Application
handleImageRequest request respond = do
    let id = last $ splitOn "/" (BS.unpack (rawPathInfo request))

    let query = queryString request :: [(ByteString, Maybe ByteString)]
        mode = fromMaybe "gif" (fmap (BS.unpack . fromMaybe "" ) (lookup "mode" query))
        force = fromMaybe "false" (fmap (BS.unpack . fromMaybe "" ) (lookup "force" query))
        speed = fromMaybe "1.0" (fmap (BS.unpack . fromMaybe "" ) (lookup "speed" query))

    make_text_response respond $ "Making image with: \n<ID>: " ++ show id
        ++ "\n<mode>: " ++ mode
        ++ "\n<force>: " ++ force
        ++ "\n<speed>: " ++ speed

-- Streaming response logic
streamingResponse :: (Response -> IO b) -> IO b
streamingResponse respond = do
    let source = CL.enumFromTo 1 10 -- Generate numbers from 1 to 10
    let responseSource = C.map (flip (,) "text/plain") source
    let responseBody = C.runConduit $ responseSource .| CL.map (flip (,) . format) .| C.awaitForever (respond . responseLBS status200 [(mk (BS.pack "Content-Type"), BS.pack "text/plain")] . LBS.pack)
    respond responseBody

-- Format function for streaming data
format :: Int -> String
format n = "Number: " ++ show n ++ "\n"

-- Function to run the server
runAPI :: Int -> IO ()
runAPI port = do
    putStrLn $ "Listening on port: " ++ show port 
    run port application
