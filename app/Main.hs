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
        return Message {updateID = updateID,
                        chatID = chatID, 
                        fromChatID = fromChatID,
                        messageID = messageID,
                        text = text}

-- ??? take it from configs
token :: ByteString
 

getUpdates :: ByteString -> IO Value
getUpdates offset = do 
            let path = "/bot" <> token <> "/getUpdates?limit=1" <> offset
            let request = setRequestPath path 
                            $ setRequestHost "api.telegram.org" 
                            $ setRequestPort 443
                            $ setRequestSecure True defaultRequest 
            response <- httpJSON request
            return $ getResponseBody response

getOffset :: Maybe Message -> ByteString
getOffset Nothing = ""
getOffset (Just mes) = Char8.pack . show $ updateID mes + 1

main :: IO ()
main = do 
    r <- getUpdates $ getOffset Nothing
    let r' = parseMaybe parseJSON r :: Maybe Message
    print r'
