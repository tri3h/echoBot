{-# LANGUAGE OverloadedStrings #-}
 
module Handlers.Tg where
 
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
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
 
data Handle m = Handle {
getToken :: m Char8.ByteString,
getHelpText :: m Char8.ByteString,
getRepeatText :: m Char8.ByteString,
getRepeatNum :: String -> m Int,
getResponse :: Request -> m (Response Value),
changeRepeatNum :: String -> String -> m ()
}
 
makeRequest :: Monad m => Handle m -> BS.ByteString -> Query -> Value -> m Request
makeRequest handle path params json = do
   token <- getToken handle
   let fullPath = "/bot" `BS.append` token `BS.append` path
   return $ setRequestBodyJSON json
           $ setRequestQueryString params
           $ setRequestPath fullPath
           $ setRequestHost "api.telegram.org"
           $ setRequestPort 443
           $ setRequestSecure True defaultRequest
 
sendHelp :: Monad m => Handle m -> BS.ByteString -> m ()
sendHelp handle chatID = do
   helpText <- getHelpText handle
   let params = [("chat_id", Just chatID),
                         ("text", Just helpText)]
   req <- makeRequest handle "/sendMessage" params Null
   getResponse handle req
   return ()
 
getUpdate :: Monad m => Handle m -> Maybe Message -> m (Maybe Message)
getUpdate handle mes = do
   let params = [("limit", Just "1"),
                         ("timeout", Just "10"),
                         ("offset", getOffset mes)]
   req <- makeRequest handle "/getUpdates" params Null
   response <- getResponse handle req
   return (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)
       where   getOffset :: Maybe Message -> Maybe BS.ByteString
               getOffset Nothing = Nothing
               getOffset (Just mes) = Just . toByteString $ updateID mes + 1
 
getUpdates :: Monad m => Handle m -> Maybe Message -> m ()
getUpdates handle mes = do
   update <- getUpdate handle mes
   case update of
       Just m -> do
           status <- chooseAnswer handle m
           getUpdates handle update
       Nothing -> getUpdates handle Nothing
 
chooseAnswer :: Monad m => Handle m -> Message -> m ()
chooseAnswer handle mes = case text mes of
               Just "/help" -> sendHelp handle chat
               Just "/repeat" -> getRepeatNumFromUser handle chat
               _ -> repeatMessage handle mes
   where chat = toByteString $ chatID mes
 
getRepeatNumFromUser :: Monad m => Handle m -> BS.ByteString -> m ()
getRepeatNumFromUser handle id = do
   oldRepeatrepeatNum <- getRepeatNum handle (Char8.unpack id)
   repeatText <- getRepeatText handle
   let params = [("chat_id", Just id),
                         ("text", Just $ repeatText `BS.append` toByteString oldRepeatrepeatNum)]
       buttons = object [ "reply_markup" .= object [
                           "keyboard" .= [["1", "2", "3", "4", "5" :: String]],
                           "resize_keyboard" .= True,
                           "one_time_keyboard" .= True
                           ]]
   req <- makeRequest handle "/sendMessage" params buttons
   response <- getResponse handle req
   confirmMes <- getUpdate handle Nothing
   repeatNumMes <- getNewRepeatNum handle confirmMes
   case text repeatNumMes of
       Just x -> changeRepeatNum handle x (Char8.unpack id)
       Nothing -> return ()
   where   getNewRepeatNum :: Monad m => Handle m -> Maybe Message -> m Message
           getNewRepeatNum handle mes = do
               newMes <- getUpdate handle mes
               case newMes of
                   Nothing -> getNewRepeatNum handle mes
                   Just x -> do
                       getUpdate handle newMes
                       return x
  
repeatMessage :: Monad m => Handle m -> Message -> m ()
repeatMessage handle mes = do
   repeatNum <- getRepeatNum handle (show $ chatID mes)
   let param = [("chat_id", chatID mes),
               ("from_chat_id", fromChatID mes),
               ("message_id", messageID mes)]
       params = fmap (\(a, b) -> (a, Just $ toByteString b)) param
   req <- makeRequest handle "/copyMessage" params Null
   send handle req repeatNum
   where   send :: Monad m => Handle m -> Request -> Int -> m ()
           send _ _ 0 = return ()
           send handle req n = do
               getResponse handle req
               send handle req $ n-1
 
toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x
