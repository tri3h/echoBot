{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import Data.ByteString
import Data.Aeson
import Data.Aeson.Types

data Message = Message { chatID :: Integer, 
                         fromChatID :: Integer,
                         messageID :: Integer,
                         text :: Maybe String } deriving Show

instance FromJSON Message where
    parseJSON = withObject "Message" $ \obj -> do
        message <- obj .: "message"
        messageID <- message .: "message_id"
        from <- message .: "from"
        fromChatID <- from .: "id"
        chat <- message .: "chat"
        chatID <- chat .: "id"
        text <- message .:? "text"
        return Message {chatID = chatID, 
                        fromChatID = fromChatID,
                        messageID = messageID,
                        text = text}

parseMessages :: Value -> Parser [Message]
parseMessages = withObject "Array of messages" $ \obj -> obj .: "result"

-- ??? take it from configs
token :: ByteString
 

getUpdates :: IO Value
getUpdates = do 
            let path = "/bot" <> token <> "/getUpdates"
            let request = setRequestPath path 
                            $ setRequestHost "api.telegram.org" 
                            $ setRequestPort 443
                            $ setRequestSecure True defaultRequest 
            response <- httpJSON request
            return $ getResponseBody response

main :: IO ()
main = do 
    r <- getUpdates
    let r' = parseMaybe parseMessages r
    print r'
