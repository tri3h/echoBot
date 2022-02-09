{-# LANGUAGE OverloadedStrings #-}

module App.Tg where

import qualified App.Handlers.Bot as H
import qualified Data.Configurator as Config
import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified Network.HTTP.Simple as Net
import qualified Data.Text as Text
import qualified Data.List as List
import Control.Monad.State.Lazy
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Aeson.Types
import qualified Data.Map as Map

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

getMessage :: Request -> IO (Maybe Message)
getMessage req = do
    response <- httpJSON req :: IO (Response Value)
    return (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)

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

getRepeatNum :: Map.Map H.UserID Integer -> Integer -> H.UserID -> Integer
getRepeatNum m defNum user = 
    case evalState (get' user) m of
        Just x -> x
        Nothing -> defNum
    where get' :: H.UserID -> State (Map.Map H.UserID Integer) (Maybe Integer) 
          get' user =  do 
              m <- get 
              return $ Map.lookup user m

setRepeatNum :: Map.Map H.UserID Integer -> H.UserID -> Integer -> ()
setRepeatNum m user num = evalState (set user num) m
    where   set :: H.UserID -> Integer -> State (Map.Map H.UserID Integer) ()
            set user num = do 
                    m <- get
                    put $ Map.insert user num m 
                    return ()

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x

main :: IO ()
main = do 
    config <- Config.load [Config.Required "TG.config"]
    token <- Config.require config "token"
    helpText <- Config.require config "help_text"
    repeatText <- Config.require config "repeat_text"
    defaultNum <- Config.require config "repeat_times.default" 
    let repeatNums = Map.empty
    let handle = H.Handle {
        H.getMessage = getMessage,
        H.makeUpdateReq = makeUpdateReq token,
        H.makeHelpReq = makeHelpReq token helpText,
        H.makeRepeatReq = makeRepeatReq token,
        H.makeRepeatQuestionReq = \mes -> do
            let num = getRepeatNum repeatNums defaultNum $ chatID mes
            makeRepeatQuestionReq token repeatText num mes,
        H.getText = \mes -> case text mes of
                            Just m -> m
                            Nothing -> "",
        H.getUserID = chatID,
        H.getRepeatNum = getRepeatNum repeatNums defaultNum,
        H.setRepeatNum = setRepeatNum repeatNums
    }
    H.getUpdate handle Nothing
    return ()
