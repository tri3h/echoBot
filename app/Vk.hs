{-# LANGUAGE OverloadedStrings #-}

module Vk where

import Data.Aeson.Types
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import qualified Data.Configurator as Config
import qualified Data.Configurator.Types as ConfigT
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import System.Exit
import System.Random

data Message = Message { tsMes :: String,
                        peerID :: Integer,
                        text :: Maybe String,
                        attachments :: Maybe [Attachment] } deriving Show
 
data Attachment = Attachment { name :: String,
                            ownerID :: Integer,
                            mediaID :: Integer,
                            accessKey :: Maybe String } deriving Show

instance FromJSON Attachment where
    parseJSON = withObject "Attachment" $ \o -> do
        name <- o .: "type"
        media <- o .: Text.pack name
        id <- media .:? "owner_id" :: Parser (Maybe Integer)
        ownerID <- case id of
                Just x -> media .: "owner_id"
                Nothing -> media .: "to_id"
        mediaID <- media .: "id"
        accessKey <- media .:? "access_key"
        return Attachment { name = name,
                            ownerID = ownerID,
                            mediaID = mediaID,
                            accessKey = accessKey}

instance FromJSON Message where
   parseJSON = withObject "Message" $ \o -> do
       ts <- o .: "ts"
       [updates] <- o .: "updates"
       object <- updates .: "object"
       message <- object .: "message"
       peerID <- message .: "peer_id"
       text <- message .:? "text"
       attachments <- message .:? "attachments"
       return Message { tsMes = ts,
                        peerID = peerID,
                        text = text,
                        attachments = attachments}

data ConnectionInfo = ConnectionInfo {  key :: String,
                                        server :: String,
                                        ts :: String }

instance FromJSON ConnectionInfo where
    parseJSON = withObject "ConnectionInfo" $ \obj -> do
        response <- obj .: "response"
        key <- response .: "key"
        server <- response .: "server"
        ts <- response .: "ts"
        return ConnectionInfo {key = key,
                                server = server,
                                ts = ts}

getConnectionInfo :: ByteString -> ByteString -> IO ConnectionInfo
getConnectionInfo token groupID = do
    let path = "/method/groups.getLongPollServer?group_id=" 
                `BS.append` groupID `BS.append` "&access_token=" 
                `BS.append` token `BS.append` "&v=5.131"
        req = setRequestPath path
            $ setRequestHost "api.vk.com"
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest
    resp <- httpJSON req :: IO (Response Value)
    let info = parseMaybe parseJSON $ getResponseBody resp :: Maybe ConnectionInfo
    case info of
        Just x -> return x
        Nothing -> exitFailure 

getUpdate :: ConnectionInfo -> String -> IO (Maybe Message)
getUpdate info ts = do
    let path = Char8.pack (drop (length ("https://lp.vk.com" :: String)) (server info))
                `BS.append` "?act=a_check&key=" `BS.append` Char8.pack (key info) `BS.append` "&ts=" 
                `BS.append` Char8.pack ts `BS.append` "&wait=25"
        req = setRequestPath path
            $ setRequestHost "lp.vk.com"
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest
    response <- httpJSON req :: IO (Response Value)
    return (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)

getUpdates :: ConnectionInfo -> ConfigT.Config -> IO ()
getUpdates info config = do
   message <- getUpdate info (ts info)
   get message
    where   get :: Maybe Message -> IO ()
            get mes = case mes of 
                        Just m -> do
                            chooseAnswer m config
                            newMes <- getUpdate info (tsMes m)
                            get newMes
                        Nothing -> getUpdates info config

chooseAnswer :: Message -> ConfigT.Config -> IO ()
chooseAnswer mes config = case text mes of
               Just "/help" -> sendHelp config mes
               Just "/repeat" -> getRepeatNumFromUser config
               _ -> repeatMessage mes config

sendHelp :: ConfigT.Config -> Message -> IO ()
sendHelp config mes = do
    helpText <- Config.require config "help_text"
    token <- Config.require config "token"
    random <- randomRIO (0, 100000000) :: IO Integer
    let param = [("message", Just helpText),
                ("peer_id", Just $ toByteString $ peerID mes),
                ("random_id", Just $ toByteString random),
                ("access_token", Just token),
                ("v", Just $ toByteString 5.131)]
        path = "/method/messages.send"
        req = setRequestPath path
            $ setRequestQueryString param
            $ setRequestHost "api.vk.com"
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest
    resp <- httpJSON req :: IO (Response Value)
    return ()

getRepeatNumFromUser :: ConfigT.Config -> IO ()
getRepeatNumFromUser = undefined

repeatMessage :: Message -> ConfigT.Config -> IO ()
repeatMessage mes config = do
    token <- Config.require config "token"
    random <- randomRIO (0, 100000000) :: IO Integer
    let botMessage = case text mes of
                        Just x -> Just $ Encoding.encodeUtf8 $ Text.pack x
                        Nothing -> Nothing
        botAttachments = case attachments mes of
                        Just x -> Just . Char8.pack $ toString x
                        Nothing -> Nothing
        param = [("message", botMessage),
                ("attachment", botAttachments),
                ("peer_id", Just $ toByteString $ peerID mes),
                ("random_id", Just $ toByteString random),
                ("access_token", Just token),
                ("v", Just $ toByteString 5.131)]
        path = "/method/messages.send"
        req = setRequestPath path
            $ setRequestQueryString param
            $ setRequestHost "api.vk.com"
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest
    resp <- httpJSON req :: IO (Response Value)
    return ()
    where   toString :: [Attachment] -> String
            toString [] = ""
            toString [x] = makeString x
            toString (x:xs) = makeString x ++ "," ++ toString xs
            makeString :: Attachment -> String
            makeString x = let maybeKey = case accessKey x of
                                            Just key -> "_" ++ key
                                            Nothing -> ""
                            in name x ++ show (ownerID x) ++ "_" ++ show (mediaID x) ++ maybeKey

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x

main :: IO ()
main = do
    config <- Config.load [Config.Required "botVK.config"]
    token <- Config.require config "token"
    groupID <- Config.require config "group_id"
    info <- getConnectionInfo token groupID
    getUpdates info config
