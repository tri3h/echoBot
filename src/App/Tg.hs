{-# LANGUAGE OverloadedStrings #-}

module App.Tg where

import qualified App.Handlers.Bot as Bot
import qualified App.Handlers.Logger as Logger
import qualified Data.Configurator as Config
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Aeson.Types
import qualified Data.Map as Map
import Data.IORef 

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

makeRequest :: BS.ByteString -> Query -> Value -> IO Request
makeRequest path params json = do
   return $ setRequestBodyJSON json
           $ setRequestQueryString params
           $ setRequestPath path
           $ setRequestHost "api.telegram.org"
           $ setRequestPort 443
           $ setRequestSecure True defaultRequest

getMessage :: Logger.Handle IO -> Request -> IO (Maybe Message)
getMessage logger req = do
    response <- httpJSON req :: IO (Response Value)
    Logger.debug logger ("Made request:\n" ++ show req)
    Logger.debug logger ("Got response:\n" ++ show response)
    let mes = (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)
    Logger.debug logger ("Parsed response and got message:\n" ++ show mes)
    return mes

makeUpdateReq :: BS.ByteString -> Maybe Message -> IO Request
makeUpdateReq token mes = do
            let params = [("limit", Just "1"),
                            ("timeout", Just "10"),
                            ("offset", getOffset mes)]
                path = "/bot" `BS.append` token `BS.append` "/getUpdates"
            makeRequest path params Null 
    where   getOffset :: Maybe Message -> Maybe BS.ByteString
            getOffset Nothing = Nothing
            getOffset (Just mes) = Just . toByteString $ updateID mes + 1

makeHelpReq :: BS.ByteString -> BS.ByteString -> Message -> IO Request
makeHelpReq token helpText mes = do
            let params = [("chat_id", Just . toByteString $ chatID mes),
                            ("text", Just helpText)]
                path = "/bot" `BS.append` token `BS.append` "/sendMessage"
            makeRequest path params Null

makeRepeatReq :: BS.ByteString -> Message -> IO Request
makeRepeatReq token mes = do 
            let param = [("chat_id", chatID mes),
                        ("from_chat_id", fromChatID mes),
                        ("message_id", messageID mes)]
                params = fmap (\(a, b) -> (a, Just $ toByteString b)) param
                path = "/bot" `BS.append` token `BS.append` "/copyMessage"
            makeRequest path params Null

makeRepeatQuestionReq :: BS.ByteString -> BS.ByteString -> Integer -> Message -> IO Request
makeRepeatQuestionReq token repeatText num mes = do
                let params = [("chat_id", Just . toByteString $ chatID mes),
                                ("text", Just $ repeatText `BS.append` toByteString num)]
                    buttons = object [ "reply_markup" .= object [
                                    "keyboard" .= [["1", "2", "3", "4", "5" :: String]],
                                    "resize_keyboard" .= True,
                                    "one_time_keyboard" .= True
                                    ]]
                    path = "/bot" `BS.append` token `BS.append` "/sendMessage"
                makeRequest path params buttons

getRepeatNum :: Logger.Handle IO -> IORef (Map.Map Bot.UserID Integer) -> Integer -> Bot.UserID -> IO Integer
getRepeatNum logger ref defNum user = do
    m <- readIORef ref
    case Map.lookup user m of
        Just x -> do
            Logger.debug logger ("Got repeat num = " ++ show x ++ " for user " ++ show user)
            return x
        Nothing -> do 
            Logger.debug logger ("Not found repeat num for user " ++ show user ++ ". Set default num")
            return defNum
    
setRepeatNum :: Logger.Handle IO -> IORef (Map.Map Bot.UserID Integer) -> Bot.UserID -> Integer -> IO ()
setRepeatNum logger ref user num = do 
    modifyIORef' ref (Map.insert user num)
    Logger.debug logger ("Set repeat num = " ++ show num ++ " for user " ++ show user)

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x

main :: IO ()
main = do 
    config <- Config.load [Config.Required "TG.config"]
    token <- Config.require config "token"
    helpText <- Config.require config "help_text"
    repeatText <- Config.require config "repeat_text"
    defaultNum <- Config.require config "repeat_times.default" 
    repeatNums <- newIORef Map.empty
    let loggerHandle = Logger.Handle {
        Logger.verbosity = Logger.Debug,
        Logger.writeLog = putStrLn
    }
    let botHandle = Bot.Handle {
        Bot.getMessage = getMessage loggerHandle,
        Bot.makeUpdateReq = makeUpdateReq token,
        Bot.makeHelpReq = makeHelpReq token helpText,
        Bot.makeRepeatReq = makeRepeatReq token,
        Bot.makeRepeatQuestionReq = \mes -> do
            num <- getRepeatNum loggerHandle repeatNums defaultNum $ chatID mes
            makeRepeatQuestionReq token repeatText num mes,
        Bot.getText = \mes -> case text mes of
                            Just m -> m
                            Nothing -> "",
        Bot.getUserID = chatID,
        Bot.getRepeatNum = getRepeatNum loggerHandle repeatNums defaultNum,
        Bot.setRepeatNum = setRepeatNum loggerHandle repeatNums
    }
    Bot.getUpdate botHandle Nothing
    return ()
