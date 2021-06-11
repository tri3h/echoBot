{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import Data.ByteString
import Data.ByteString.Char8 as Char8
import Data.Aeson
import Data.Aeson.Types

data Message = Message { updateID :: Integer,
                         chatID :: Integer, 
                         fromChatID :: Integer,
                         messageID :: Integer,
                         text :: Maybe String } deriving Show

instance FromJSON Message where
    parseJSON = withObject "Message" $ \obj -> do
        [result] <- obj .: "result"
        updateID <- result .: "update_id"
        message <- result .: "message"
        messageID <- message .: "message_id"
        from <- message .: "from"
        fromChatID <- from .: "id"
        chat <- message .: "chat"
        chatID <- chat .: "id"
        text <- message .:? "text"
        return Message { updateID = updateID,
                         chatID = chatID, 
                         fromChatID = fromChatID,
                         messageID = messageID,
                         text = text }

-- ??? take it from configs
token :: ByteString
 

sendRequest :: ByteString -> Query -> IO Value
sendRequest path param = do 
            let fullPath = "/bot" <> token <> path
            let request = setRequestQueryString param
                            $ setRequestPath fullPath 
                            $ setRequestHost "api.telegram.org" 
                            $ setRequestPort 443
                            $ setRequestSecure True defaultRequest 
            response <- httpJSON request
            return $ getResponseBody response

getUpdate :: Maybe Message -> IO Value 
getUpdate mes = sendRequest "/getUpdates" param 
    where param = [("limit", Just "1"), ("offset", getOffset mes)] 
          getOffset :: Maybe Message -> Maybe ByteString
          getOffset Nothing = Nothing
          getOffset (Just mes) = Just . Char8.pack . show $ updateID mes + 1

main :: IO ()
main = do 
    r <- getUpdate Nothing
    let r' = parseMaybe parseJSON r :: Maybe Message
    print r'
