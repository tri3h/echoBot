{-# LANGUAGE OverloadedStrings #-}

module App.Vk where

import qualified App.Handlers.Bot as H 
import Data.Aeson.Types
import Data.Aeson ( encode )
import Network.HTTP.Simple
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Internal ( ByteString )
import qualified Data.Configurator as Config
import qualified Data.Configurator.Types as C
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified Data.List as List
import System.Exit ( exitFailure )
import System.Random ( Random(randomRIO) )

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

getMessage :: Request -> IO (Maybe Message)
getMessage req = do
    response <- httpJSON req :: IO (Response Value)
    return (parseMaybe parseJSON $ getResponseBody response :: Maybe Message)

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

makeHelpReq :: ByteString -> ByteString -> Message -> IO Request 
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

makeRepeatReq :: ByteString -> Message -> IO Request
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
        
mediaToBS :: Message -> Maybe ByteString
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

stickerToBS :: Message -> Maybe ByteString
stickerToBS mes = case attachments mes of
            Just x -> Just $ getSticker x
            Nothing -> Nothing
    where getSticker :: [Attachment] -> ByteString
          getSticker [] = ""
          getSticker (Sticker s: xs) = toByteString s
          getSticker (_:xs) = getSticker xs

makeRepeatQuestionReq :: ByteString -> ByteString -> Integer -> Message -> IO Request 
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
    config <- Config.load [Config.Required "botVK.config"]
    token <- Config.require config "token"
    groupID <- Config.require config "group_id"
    helpText <- Config.require config "help_text"
    repeatText <- Config.require config "repeat_text"
    defaultNum <- Config.require config "repeat_times.default" 
    info <- getConnectionInfo token groupID
    let repeatNums = Map.empty
    let handle = H.Handle {
    H.getMessage = getMessage,
    H.makeUpdateReq = makeUpdateReq info,
    H.makeHelpReq = makeHelpReq token helpText,
    H.makeRepeatReq = makeRepeatReq token,
    H.makeRepeatQuestionReq = \mes -> do
        let num = getRepeatNum repeatNums defaultNum $ peerID mes
        makeRepeatQuestionReq token repeatText num mes,
    H.getText = \mes -> case text mes of
                            Just m -> m
                            Nothing -> "",
    H.getUserID = peerID,
    H.getRepeatNum = getRepeatNum repeatNums defaultNum,
    H.setRepeatNum = setRepeatNum repeatNums
    }
    H.getUpdate handle Nothing
    return ()