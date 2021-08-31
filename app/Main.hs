{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Configurator as Conf

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

data Config = Config { token :: ByteString,
                       helpText :: ByteString,
                       repeatText :: ByteString,
                       repeatTimes :: Int }

makeRequest :: Config -> ByteString -> Query -> Value -> Request 
makeRequest conf path params json = let fullPath = "/bot" `append` token conf `append` path
                         in setRequestBodyJSON json
                            $ setRequestQueryString params
                            $ setRequestPath fullPath 
                            $ setRequestHost "api.telegram.org" 
                            $ setRequestPort 443
                            $ setRequestSecure True defaultRequest 

getUpdate :: Config -> Maybe Message -> IO (Maybe Message)
getUpdate conf mes = do 
    response <- httpJSON req
    let newMessage = parseMaybe parseJSON $ getResponseBody response :: Maybe Message
    return newMessage
        where   req = makeRequest conf "/getUpdates" params Null 
                params = [("limit", Just "1"),
                          ("timeout", Just "10"), 
                          ("offset", getOffset mes)] 
                getOffset :: Maybe Message -> Maybe ByteString
                getOffset Nothing = Nothing
                getOffset (Just mes) = Just . toByteString $ updateID mes + 1

sendHelp :: Config -> ByteString -> IO Int
sendHelp conf chatID = do 
    response <- httpJSON req :: IO (Response Value)
    return $ getResponseStatusCode response 
        where   req = makeRequest conf "/sendMessage" params Null
                params = [("chat_id", Just chatID), 
                          ("text", Just (helpText conf))]

sendRepeatQuestion :: Config -> ByteString -> IO Int
sendRepeatQuestion conf chatID = do 
    response <- httpJSON req :: IO (Response Value)
    return $ getResponseStatusCode response 
        where   req = makeRequest conf "/sendMessage" params buttons
                params = [("chat_id", Just chatID), 
                          ("text", Just $ repeatText conf)]
                buttons :: Value
                buttons = object [ "reply_markup" .= object [
                            "keyboard" .= [["1", "2", "3", "4", "5" :: String]],
                            "resize_keyboard" .= True,
                            "one_time_keyboard" .= True
                            ]]

repeatMessage :: Config -> Message -> IO Int
repeatMessage conf mes = send times
    where   send :: Int -> IO Int
            send 0 = return (200 :: Int)
            send n = do 
                response <- httpJSON req :: IO (Response Value)
                let status = getResponseStatusCode response
                if status == 200 then send $ n-1 else return status
            req =  makeRequest conf "/copyMessage" finalParams Null
            finalParams = fmap (\(a, b) -> (a, Just $ toByteString b)) params
            params = [("chat_id", chatID mes), 
                    ("from_chat_id", fromChatID mes),
                    ("message_id", messageID mes)]
            times = repeatTimes conf

chooseAnswer :: Config -> Message -> IO Int
chooseAnswer conf mes = case text mes of
                Just "/help" -> sendHelp conf chat
                Just "/repeat" -> sendRepeatQuestion conf chat
                _ -> repeatMessage conf mes 
    where chat = toByteString $ chatID mes

toByteString :: Show a => a -> ByteString 
toByteString x = Char8.pack $ show x

getUpdates :: Config -> Maybe Message -> IO ()
getUpdates conf mes = do
    update <- getUpdate conf mes
    case update of
        Just m -> do
            status <- chooseAnswer conf m
            print status
            getUpdates conf update
        Nothing -> getUpdates conf Nothing

main :: IO ()
main = do 
    config <- Conf.load [Conf.Required "bot.config"]
    confToken <- Conf.require config "token"
    confHelpText <- Conf.require config "help_text"
    confRepeatText <- Conf.require config "repeat_text"
    confRepeatTimes <- Conf.require config "repeat_times"
    let myConfig = Config {token = confToken, helpText = confHelpText, repeatText = confRepeatText, repeatTimes = confRepeatTimes}
    getUpdates myConfig Nothing
    Conf.display config

