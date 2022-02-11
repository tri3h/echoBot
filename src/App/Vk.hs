{-# LANGUAGE OverloadedStrings #-}

module App.Vk where

import qualified App.Handlers.Bot as Bot 
import qualified App.Handlers.Logger as Logger
import Data.Aeson.Types
import Data.Aeson ( encode )
import Network.HTTP.Simple
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as Config
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import System.Exit ( exitFailure )
import System.Random ( Random(randomRIO) )
import Data.IORef 

data Message = Message { tsMes :: String,
                        peerID :: Integer,
                        text :: Maybe String,
                        forward :: Maybe [ForwardID],
                        geo :: Maybe GeoInfo,
                        attachments :: Maybe [Attachment] } deriving Show

data Attachment = Media { name :: String,
                            ownerID :: Integer,
                            mediaID :: Integer,
                            accessKey :: Maybe String } 
                | Sticker { stickerID :: Integer}
                deriving Show

data ConnectionInfo = ConnectionInfo {  key :: String,
                                        server :: String,
                                        ts :: String } deriving Show

data GeoInfo = GeoInfo { lat :: Float,
                        long :: Float } deriving Show

newtype ForwardID = ForwardID { forwardID :: Integer } deriving Show

instance FromJSON Message where
   parseJSON = withObject "Message" $ \o -> do
       ts <- o .: "ts"
       [updates] <- o .: "updates"
       object <- updates .: "object"
       message <- object .: "message"
       peerID <- message .: "peer_id"
       forward <- message .:? "fwd_messages"
       geo <- message .:? "geo"
       text <- message .:? "text"
       attachments <- message .:? "attachments"
       return Message { tsMes = ts,
                        peerID = peerID,
                        forward = forward,
                        geo = geo,
                        text = text,
                        attachments = attachments}

instance FromJSON ForwardID where
    parseJSON = withObject "ForwardID" $ \o -> do
        id <- o .: "id"
        return $ ForwardID id

instance FromJSON GeoInfo where
    parseJSON = withObject "GeoInfo" $ \o -> do
        coord <- o .: "coordinates"
        lat <- coord .: "latitude"
        long <- coord .: "longitude"
        return $ GeoInfo { lat = lat,
                        long = long }

instance FromJSON Attachment where
    parseJSON = withObject "Attachment" $ \o -> do
        name <- o .: "type"
        media <- o .: Text.pack name
        case name of 
            "sticker" -> do
                id <- media .: "sticker_id"
                return Sticker { stickerID = id }
            _ -> do
                id <- media .:? "owner_id" :: Parser (Maybe Integer)
                ownerID <- case id of
                    Just x -> media .: "owner_id"
                    Nothing -> media .: "to_id"
                mediaID <- media .: "id"
                accessKey <- media .:? "access_key"
                return Media { name = name,
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

getConnectionInfo :: Logger.Handle IO -> BS.ByteString -> BS.ByteString -> IO ConnectionInfo
getConnectionInfo logger token groupID = do
    req <- makeConnectionInfoReq token groupID
    resp <- httpJSON req :: IO (Response Value)
    let info = parseMaybe parseJSON $ getResponseBody resp :: Maybe ConnectionInfo
    case info of
        Just x -> do 
            Logger.debug logger ("Got connection information:\n" ++ show x)
            return x
        Nothing -> do
            Logger.error logger "Connection information is not found"
            exitFailure

makeConnectionInfoReq :: BS.ByteString -> BS.ByteString -> IO Request
makeConnectionInfoReq token groupID = do
    let path = "/method/groups.getLongPollServer"
        host = "api.vk.com"
        param = [("group_id", Just groupID),
                ("access_token", Just token),
                ("v", Just "5.131")]
    makeRequest path host param

getMessage :: Logger.Handle IO -> Request -> IO (Maybe Message)
getMessage logger req = do
    response <- httpJSON req :: IO (Response Value)
    Logger.debug logger ("Made request:\n" ++ show req)
    Logger.debug logger ("Got response:\n" ++ show response)
    let mes = (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)
    Logger.debug logger ("Parsed response and got message:\n" ++ show mes)
    return mes

makeRequest :: BS.ByteString -> BS.ByteString -> Query -> IO Request
makeRequest path host param = do
    return $ setRequestQueryString param
            $ setRequestPath path
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True defaultRequest

makeUpdateReq :: ConnectionInfo -> Maybe Message -> IO Request
makeUpdateReq info mes = do
    let ts' = case mes of
            Just m -> tsMes m
            Nothing -> ts info
        path = Char8.pack (drop (length ("https://lp.vk.com" :: String)) (server info))
        host = "lp.vk.com"
        param = [("act", Just "a_check"),
                ("key", Just . Char8.pack $ key info),
                ("ts", Just $ Char8.pack ts'),
                ("wait", Just "25")]
    makeRequest path host param

makeHelpReq :: BS.ByteString -> BS.ByteString -> Message -> IO Request 
makeHelpReq token helpText mes = do
    random <- randomRIO (0, 100000000) :: IO Integer
    let param = [("message", Just helpText),
                ("peer_id", Just $ toByteString $ peerID mes),
                ("random_id", Just $ toByteString random),
                ("access_token", Just token),
                ("v", Just $ toByteString 5.131)]
        path = "/method/messages.send"
        host = "api.vk.com"
    makeRequest path host param

makeRepeatReq :: BS.ByteString -> Message -> IO Request
makeRepeatReq token mes = do
    random <- randomRIO (0, 100000000) :: IO Integer
    let botMessage = case text mes of
            Just x -> Just $ Encoding.encodeUtf8 $ Text.pack x
            Nothing -> Nothing
        botForwardMessage = case forward mes of
            Just x -> (Just . Char8.pack) (concatMap ((\x -> show x ++ ",") . forwardID) x)
            Nothing -> Nothing
        latitude = case geo mes of
            Just x -> Just . toByteString $ lat x
            Nothing -> Nothing
        longitude = case geo mes of
            Just x -> Just . toByteString $ long x
            Nothing -> Nothing
        param = [("message", botMessage),
                ("attachment", mediaToBS mes),
                ("forward_messages", botForwardMessage),
                ("peer_id", Just . toByteString $ peerID mes),
                ("random_id", Just $ toByteString random),
                ("access_token", Just token),
                ("sticker_id", stickerToBS mes),
                ("lat", latitude),
                ("long", longitude),
                ("v", Just $ toByteString 5.131)]
        path = "/method/messages.send"
        host = "api.vk.com"
    makeRequest path host param
        
mediaToBS :: Message -> Maybe BS.ByteString
mediaToBS mes = case attachments mes of
            Just x -> Just . Char8.pack . mediaToString $ getMedia x
            Nothing -> Nothing
    where mediaToString :: [Attachment] -> String
          mediaToString [] = ""
          mediaToString [x] = makeString x
          mediaToString (x:xs) = makeString x ++ "," ++ mediaToString xs
          makeString :: Attachment -> String
          makeString x = let maybeKey = case accessKey x of
                                        Just key -> "_" ++ key
                                        Nothing -> ""
                            in name x ++ show (ownerID x) ++ "_" ++ show (mediaID x) ++ maybeKey
          getMedia :: [Attachment] -> [Attachment]
          getMedia [] = []
          getMedia (m@Media {} : xs) = m : getMedia xs
          getMedia (_:xs) = getMedia xs

stickerToBS :: Message -> Maybe BS.ByteString
stickerToBS mes = case attachments mes of
            Just x -> Just $ getSticker x
            Nothing -> Nothing
    where getSticker :: [Attachment] -> BS.ByteString
          getSticker [] = ""
          getSticker (Sticker s: xs) = toByteString s
          getSticker (_:xs) = getSticker xs

makeRepeatQuestionReq :: BS.ByteString -> BS.ByteString -> Integer -> Message -> IO Request 
makeRepeatQuestionReq token repeatText repeatNum mes = do
    random <- randomRIO (0, 100000000) :: IO Integer
    let param = [("keyboard", Just $ BSL.toStrict $ encode buttons),
                ("message", Just $ repeatText `BS.append` toByteString repeatNum),
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
    makeRequest path host param

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
    config <- Config.load [Config.Required "VK.config"]
    token <- Config.require config "token"
    groupID <- Config.require config "group_id"
    helpText <- Config.require config "help_text"
    repeatText <- Config.require config "repeat_text"
    defaultNum <- Config.require config "repeat_times.default" 
    repeatNums <- newIORef Map.empty
    let loggerHandle = Logger.Handle {
        Logger.verbosity = Logger.Debug,
        Logger.writeLog = putStrLn
    }
    info <- getConnectionInfo loggerHandle token groupID
    let botHandle = Bot.Handle {
    Bot.getMessage = getMessage loggerHandle,
    Bot.makeUpdateReq = makeUpdateReq info,
    Bot.makeHelpReq = makeHelpReq token helpText,
    Bot.makeRepeatReq = makeRepeatReq token,
    Bot.makeRepeatQuestionReq = \mes -> do
        num <- getRepeatNum loggerHandle repeatNums defaultNum $ peerID mes
        makeRepeatQuestionReq token repeatText num mes,
    Bot.getText = \mes -> case text mes of
                            Just m -> m
                            Nothing -> "",
    Bot.getUserID = peerID,
    Bot.getRepeatNum = getRepeatNum loggerHandle repeatNums defaultNum,
    Bot.setRepeatNum = setRepeatNum loggerHandle repeatNums
    }
    Bot.getUpdate botHandle Nothing
    return ()