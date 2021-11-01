{-# LANGUAGE OverloadedStrings #-}

module Vk where

import Data.Aeson.Types
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import qualified Data.Configurator as Config
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import System.Exit
import System.Random

data Message = Message { tsMes :: String,
                        peerID :: Integer,
                        text :: Maybe String,
                        attachments :: Maybe Array } deriving Show
 
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

getUpdate :: ConnectionInfo -> IO (Maybe Message)
getUpdate info = do
    let path = Char8.pack (drop (length ("https://lp.vk.com" :: String)) (server info))
                `BS.append` "?act=a_check&key=" `BS.append` Char8.pack (key info) `BS.append` "&ts=" 
                `BS.append` Char8.pack (ts info) `BS.append` "&wait=25"
        req = setRequestPath path
            $ setRequestHost "lp.vk.com"
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest
    response <- httpJSON req :: IO (Response Value)
    return (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)

repeatMessage :: Message -> ByteString -> IO ()
repeatMessage mes token = do
    random <- randomRIO (10000, 10000000) :: IO Integer
    let t = case text mes of
                Just x -> x
        param = [("message", Just $ Encoding.encodeUtf8 $ Text.pack t),
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

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x

main :: IO ()
main = do
    config <- Config.load [Config.Required "botVK.config"]
    token <- Config.require config "token"
    groupID <- Config.require config "group_id"
    info <- getConnectionInfo token groupID
    upd <- getUpdate info
    case upd of
        Just x -> repeatMessage x token

