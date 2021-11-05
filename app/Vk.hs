{-# LANGUAGE OverloadedStrings #-}

module Vk where

import Data.Aeson.Types
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Internal
import qualified Data.Configurator as Config
import qualified Data.Configurator.Types as ConfigT
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified Data.List as List
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

data ConnectionInfo = ConnectionInfo {  key :: String,
                                        server :: String,
                                        ts :: String }

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

instance FromJSON ConnectionInfo where
    parseJSON = withObject "ConnectionInfo" $ \obj -> do
        response <- obj .: "response"
        key <- response .: "key"
        server <- response .: "server"
        ts <- response .: "ts"
        return ConnectionInfo {key = key,
                                server = server,
                                ts = ts}

makeRequest :: BS.ByteString -> BS.ByteString -> Query -> IO Request
makeRequest path host param = do
    return $ setRequestQueryString param
            $ setRequestPath path
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest

getConnectionInfo :: ByteString -> ByteString -> IO ConnectionInfo
getConnectionInfo token groupID = do
    let path = "/method/groups.getLongPollServer"
        host = "api.vk.com"
        param = [("group_id", Just groupID),
                ("access_token", Just token),
                ("v", Just "5.131")]
    req <- makeRequest path host param
    resp <- httpJSON req :: IO (Response Value)
    let info = parseMaybe parseJSON $ getResponseBody resp :: Maybe ConnectionInfo
    case info of
        Just x -> return x
        Nothing -> exitFailure 

getUpdate :: ConnectionInfo -> String -> IO (Maybe Message)
getUpdate info ts = do
    let path = Char8.pack (drop (length ("https://lp.vk.com" :: String)) (server info))
        host = "lp.vk.com"
        param = [("act", Just "a_check"),
                ("key", Just . Char8.pack $ key info),
                ("ts", Just $ Char8.pack ts),
                ("wait", Just "25")]
    req <- makeRequest path host param
    response <- httpJSON req :: IO (Response Value)
    return (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)

getUpdates :: ConnectionInfo -> ConfigT.Config -> IO ()
getUpdates info config = do
   message <- getUpdate info (ts info)
   get message
    where   get :: Maybe Message -> IO ()
            get mes = case mes of 
                        Just m -> do
                            chooseAnswer config info m
                            newMes <- getUpdate info (tsMes m)
                            get newMes
                        Nothing -> getUpdates info config

chooseAnswer :: ConfigT.Config -> ConnectionInfo -> Message -> IO ()
chooseAnswer config info mes = case text mes of
               Just "/help" -> sendHelp config mes
               Just "/repeat" -> getRepeatNumFromUser config info mes
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
        host = "api.vk.com"
    req <- makeRequest path host param
    resp <- httpJSON req :: IO (Response Value)
    return ()

getRepeatNumFromUser :: ConfigT.Config -> ConnectionInfo -> Message -> IO ()
getRepeatNumFromUser config info mes = do
    oldRepeat <- getRepeatNum config (show $ peerID mes)
    token <- Config.require config "token"
    repeatText <- Config.require config "repeat_text"
    random <- randomRIO (0, 100000000) :: IO Integer
    let param = [("keyboard", Just $ BSL.toStrict $ encode buttons),
                ("message", Just $ repeatText `BS.append` toByteString oldRepeat),
                ("peer_id", Just $ toByteString $ peerID mes),
                ("random_id", Just $ toByteString random),
                ("access_token", Just token),
                ("v", Just $ toByteString 5.131)]
        buttons =  object ["one_time" .= True,
                            "buttons" .= [[
                                object ["action" .= object 
                                            ["type" .= ("text" :: String),
                                            "label" .= ("1" :: String)]],
                                object ["action" .= object 
                                            ["type" .= ("text" :: String),
                                            "label" .= ("2" :: String)]],
                                object ["action" .= object 
                                            ["type" .= ("text" :: String),
                                            "label" .= ("3" :: String)]],
                                object ["action" .= object 
                                            ["type" .= ("text" :: String),
                                            "label" .= ("4" :: String)]],
                                object ["action" .= object 
                                            ["type" .= ("text" :: String),
                                            "label" .= ("5" :: String)]]
                                            ]]
                            ]
        path = "/method/messages.send"
        host = "api.vk.com"
    req <- makeRequest path host param
    confirmMes <- httpJSON req :: IO (Response Value)
    repeatNumMes <- getNewRepeatNum info (tsMes mes)
    case text repeatNumMes of
        Just x -> changeRepeatNum config x (show $ peerID repeatNumMes)
        Nothing -> return ()
    where   getNewRepeatNum :: ConnectionInfo -> String -> IO Message
            getNewRepeatNum info ts = do
                newMes <- getUpdate info ts
                case newMes of
                    Nothing -> getNewRepeatNum info ts
                    Just x -> return x 

getRepeatNum :: ConfigT.Config -> String -> IO Int 
getRepeatNum config id = do
    let path = "repeat_times.id" ++ id
    userTimes <- Config.lookup config $ Text.pack path
    case userTimes of 
        Just x -> return x
        Nothing -> Config.require config "repeat_times.default"

changeRepeatNum :: ConfigT.Config -> String -> String -> IO ()
changeRepeatNum config val id = do
           let path = "botVK.config"
               name = "id" ++ id
           handle <- IO.openFile path IO.ReadMode
           contents <- IO.hGetContents handle
           let linedContents = lines contents
               indexUser = List.findIndex (List.isInfixOf name) linedContents
               indexDefault = List.findIndex (List.isInfixOf "default") linedContents
               newContents = case indexUser of
                       Just x -> let (a, b) = splitAt x linedContents
                                in unlines $ a ++ [name ++ " = " ++ val] ++ tail b
                       Nothing -> case indexDefault of
                                   Just x -> let (a, b) = splitAt x linedContents
                                       in unlines $ a ++ [name ++ " = " ++ val] ++ b
                                   Nothing -> contents
           (tempName, tempHandle) <- IO.openTempFile "." "temp"
           IO.hPutStr tempHandle newContents
           IO.hClose handle
           IO.hClose tempHandle
           Dir.removeFile path
           Dir.renameFile tempName path
           Config.reload config

repeatMessage :: Message -> ConfigT.Config -> IO ()
repeatMessage mes config = do
    repeatNum <- getRepeatNum config (show $ peerID mes)
    send config mes repeatNum
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

            send :: ConfigT.Config -> Message -> Int -> IO ()
            send _ _ 0 = return ()
            send config mes n = do
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
                    host = "api.vk.com"
                req <- makeRequest path host param
                httpJSON req :: IO (Response Value)
                send config mes $ n-1

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x

main :: IO ()
main = do
    config <- Config.load [Config.Required "botVK.config"]
    token <- Config.require config "token"
    groupID <- Config.require config "group_id"
    info <- getConnectionInfo token groupID
    getUpdates info config
