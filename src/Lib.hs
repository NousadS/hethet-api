{-# LANGUAGE OverloadedStrings #-}

module Lib ( 
    runPetPetAPI
) where

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.CaseInsensitive (mk)
import qualified Data.ByteString.Char8 as BS

application _ respond = respond $
  responseLBS status200 [(mk (BS.pack "Content-Type"), BS.pack "text/plain")] (LBS.pack "Hello World")

runPetPetAPI :: IO ()
runPetPetAPI = run 8000 application
