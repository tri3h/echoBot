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
 

makeRequest :: ByteString -> Query -> Value -> Request 
makeRequest path params json = let fullPath = "/bot" <> token <> path
                         in setRequestBodyJSON json
                            $ setRequestQueryString params
                            $ setRequestPath fullPath 
                            $ setRequestHost "api.telegram.org" 
                            $ setRequestPort 443
                            $ setRequestSecure True defaultRequest 

getUpdate :: Maybe Message -> IO (Maybe Message)
getUpdate mes = do 
    response <- httpJSON req
    let newMessage = parseMaybe parseJSON $ getResponseBody response :: Maybe Message
    return newMessage
        where   req = makeRequest "/getUpdates" params Null 
                params = [("limit", Just "1"), 
                          ("offset", getOffset mes)] 
                getOffset :: Maybe Message -> Maybe ByteString
                getOffset Nothing = Nothing
                getOffset (Just mes) = Just . toByteString $ updateID mes + 1

sendMessage :: ByteString -> ByteString -> IO Int
sendMessage chatID text = do 
    response <- httpJSON req :: IO (Response Value)
    return $ getResponseStatusCode response 
        where   req = makeRequest "/sendMessage" params Null
                params = [("chat_id", Just chatID), 
                          ("text", Just text)]

repeatMessage :: Message -> IO Int
repeatMessage mes = do 
    response <- httpJSON req :: IO (Response Value)
    return $ getResponseStatusCode response 
        where   req =  makeRequest "/copyMessage" finalParams Null
                finalParams = fmap (\(a, b) -> (a, Just $ toByteString b)) params
                params = [("chat_id", chatID mes), 
                          ("from_chat_id", fromChatID mes),
                          ("message_id", messageID mes)]

toByteString :: Show a => a -> ByteString 
toByteString x = Char8.pack $ show x

main :: IO ()
main = do 
    r <- getUpdate Nothing
    print r
