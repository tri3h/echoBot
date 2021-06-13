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
 

helpText :: ByteString 
helpText = "No help for you"

repeatText :: ByteString 
repeatText = "Choose your destiny mortal"

repeatTimes :: Int
repeatTimes = 3

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

sendHelp :: ByteString -> IO Int
sendHelp chatID = do 
    response <- httpJSON req :: IO (Response Value)
    return $ getResponseStatusCode response 
        where   req = makeRequest "/sendMessage" params Null
                params = [("chat_id", Just chatID), 
                          ("text", Just helpText)]

sendRepeatQuestion :: ByteString -> IO Int
sendRepeatQuestion chatID = do 
    response <- httpJSON req :: IO (Response Value)
    return $ getResponseStatusCode response 
        where   req = makeRequest "/sendMessage" params buttons
                params = [("chat_id", Just chatID), 
                          ("text", Just repeatText)]

buttons :: Value
buttons = object [ "reply_markup" .= object [
    "keyboard" .= [["1", "2", "3", "4", "5" :: String]],
    "resize_keyboard" .= True,
    "one_time_keyboard" .= True
    ]]

repeatMessage :: Int -> Message -> IO Int
repeatMessage n mes = send n
    where   send :: Int -> IO Int
            send 0 = return (200 :: Int)
            send n = do 
                response <- httpJSON req :: IO (Response Value)
                let status = getResponseStatusCode response
                if status == 200 then send $ n-1 else return status
            req =  makeRequest "/copyMessage" finalParams Null
            finalParams = fmap (\(a, b) -> (a, Just $ toByteString b)) params
            params = [("chat_id", chatID mes), 
                    ("from_chat_id", fromChatID mes),
                    ("message_id", messageID mes)]

chooseAnswer :: Message -> IO Int
chooseAnswer mes = case text mes of
                Just "/help" -> sendHelp chat
                Just "/repeat" -> sendRepeatQuestion chat
                _ -> repeatMessage repeatTimes mes
    where chat = toByteString $ chatID mes

toByteString :: Show a => a -> ByteString 
toByteString x = Char8.pack $ show x

main :: IO ()
main = do 
    r <- getUpdate Nothing
    result <- case r of
        Just mes -> chooseAnswer mes
    print result
