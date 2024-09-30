{-# LANGUAGE OverloadedStrings #-}

-- Сервер, написан ручками, работает

module OldServer ( 
    runAPI
) where

import           Control.Monad.Trans.Resource


import Network.Wai
import Network.HTTP.Types (status200)
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

make_text_response :: (Response -> b) -> [Char] -> b
make_text_response respond text = respond $ responseLBS status200 [(mk (BS.pack "Content-Type"), BS.pack "text/plain")] (LBS.pack text)

application :: Application
application request respond = do
    putStrLn $ "New request: " ++ BS.unpack (rawPathInfo request)
    
    let id = last $ splitOn "/" (BS.unpack (rawPathInfo request))

    let query = queryString request :: [(ByteString, Maybe ByteString)]
        mode = fromMaybe "gif" (fmap (BS.unpack . fromMaybe "" ) (lookup "mode" query))
        force = fromMaybe "false" (fmap (BS.unpack . fromMaybe "" ) (lookup "force" query))
        speed = fromMaybe "1.0" (fmap (BS.unpack . fromMaybe "" ) (lookup "speed" query))

    make_text_response respond $ "Making image with: \n<ID>: " ++ show id
        ++ "\n<mode>: " ++ mode
        ++ "\n<force>: " ++ force
        ++ "\n<speed>: " ++ speed

runAPI :: Int -> IO ()
runAPI port = do
    putStrLn $ "Listening on port: " ++ show port 
    run port application