{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import Data.ByteString

-- ??? take it from configs
token :: ByteString
 

-- ??? httpJSON instead of httpBS
getUpdates :: IO ByteString 
getUpdates = do 
            let path = "/bot" <> token <> "/getUpdates"
            let request = setRequestPath path 
                            $ setRequestHost "api.telegram.org" 
                            $ setRequestPort 443
                            $ setRequestSecure True defaultRequest 
            response <- httpBS request
            return $ getResponseBody response

main :: IO ()
main = do 
    r <- getUpdates
    print r