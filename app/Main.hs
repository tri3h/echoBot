{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Aeson
import Data.Aeson.Types
import qualified Data.List as List
import qualified Data.Configurator as Config
import qualified Data.Configurator.Types as ConfigT
import qualified System.IO as IO
import qualified System.Directory as Dir

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

makeRequest :: ConfigT.Config -> BS.ByteString -> Query -> Value -> IO Request 
makeRequest conf path params json = do
    token <- Config.require conf "token"
    let fullPath = "/bot" `BS.append` token `BS.append` path
    return $ setRequestBodyJSON json
            $ setRequestQueryString params
            $ setRequestPath fullPath 
            $ setRequestHost "api.telegram.org" 
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest 


sendHelp :: ConfigT.Config -> BS.ByteString -> IO Int
sendHelp conf chatID = do 
    helpText <- Config.require conf "help_text"
    let params = [("chat_id", Just chatID), 
                          ("text", Just helpText)]
    req <- makeRequest conf "/sendMessage" params Null
    response <- httpJSON req :: IO (Response Value)
    return $ getResponseStatusCode response

sendRepeatQuestion :: ConfigT.Config -> BS.ByteString -> IO Int
sendRepeatQuestion conf chatID = do 
    repeatText <- Config.require conf "repeat_text"
    let params = [("chat_id", Just chatID), 
                          ("text", Just repeatText)]
    let buttons = object [ "reply_markup" .= object [
                            "keyboard" .= [["1", "2", "3", "4", "5" :: String]],
                            "resize_keyboard" .= True,
                            "one_time_keyboard" .= True
                            ]]    
    req <- makeRequest conf "/sendMessage" params buttons
    httpJSON req :: IO (Response Value)
    confirmMes <- getUpdate conf Nothing
    repeatTimesMes <- getRepeatTimesMes confirmMes
    case text repeatTimesMes of
        Nothing -> return (400 :: Int)
        Just x -> changeRepeatTimes x conf
    return 200
        where   getRepeatTimesMes :: Maybe Message -> IO Message
                getRepeatTimesMes mes = do 
                    newMes <- getUpdate conf mes
                    case newMes of
                        Nothing -> getRepeatTimesMes mes
                        Just x -> return x

changeRepeatTimes :: String -> ConfigT.Config -> IO Int
changeRepeatTimes val conf = do
    let path = "bot.config"
    handle <- IO.openFile path IO.ReadMode 
    (tempName, tempHandle) <- IO.openTempFile "." "temp"
    contents <- IO.hGetContents handle
    let linedContents = lines contents
        index = List.findIndex (List.isPrefixOf "repeat_times") linedContents
        newContents = case index of 
                        Just x -> let (a, b) = splitAt x linedContents 
                                 in unlines $ a ++ ["repeat_times = " ++ val] ++ tail b
                        Nothing -> contents
    IO.hPutStr tempHandle newContents
    IO.hClose handle
    IO.hClose tempHandle
    Dir.removeFile path
    Dir.renameFile tempName path
    Config.reload conf
    return 200

repeatMessage :: ConfigT.Config -> Message -> IO Int
repeatMessage conf mes = do 
    times <- Config.require conf "repeat_times"
    send times
    where   send :: Int -> IO Int
            send 0 = return (200 :: Int)
            send n = do 
                let param = [("chat_id", chatID mes), 
                            ("from_chat_id", fromChatID mes),
                            ("message_id", messageID mes)]
                let params = fmap (\(a, b) -> (a, Just $ toByteString b)) param
                req <- makeRequest conf "/copyMessage" params Null
                response <- httpJSON req :: IO (Response Value)
                let status = getResponseStatusCode response
                if status == 200 then send $ n-1 else return status
            
chooseAnswer :: ConfigT.Config -> Message -> IO Int
chooseAnswer conf mes = case text mes of
                Just "/help" -> sendHelp conf chat
                Just "/repeat" -> sendRepeatQuestion conf chat
                _ -> repeatMessage conf mes 
    where chat = toByteString $ chatID mes

toByteString :: Show a => a -> BS.ByteString 
toByteString x = Char8.pack $ show x

getUpdate :: ConfigT.Config -> Maybe Message -> IO (Maybe Message)
getUpdate conf mes = do 
    let params = [("limit", Just "1"),
                          ("timeout", Just "10"), 
                          ("offset", getOffset mes)] 
    req <-makeRequest conf "/getUpdates" params Null 
    response <- httpJSON req
    let newMessage = parseMaybe parseJSON $ getResponseBody response :: Maybe Message
    return newMessage
        where   getOffset :: Maybe Message -> Maybe BS.ByteString
                getOffset Nothing = Nothing
                getOffset (Just mes) = Just . toByteString $ updateID mes + 1

getUpdates :: ConfigT.Config -> Maybe Message -> IO ()
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
    config <- Config.load [Config.Required "bot.config"]
    getUpdates config Nothing
    Config.display config

