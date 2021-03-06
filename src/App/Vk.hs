{-# LANGUAGE OverloadedStrings #-}

module App.Vk where

import App.Config (load)
import qualified App.Handlers.Bot as Bot
import qualified App.Handlers.Logger as Logger
import App.Types.Bot
  ( BotState,
    MessageText (MessageText),
    Path (..),
    RepeatNum (..),
    UserID (UserID),
  )
import App.Types.Config (Config (..), GroupID (GroupID), HelpText (HelpText), RepeatText (RepeatText), Token (Token))
import App.Types.Vk
  ( Attachment (Media, Sticker, accessKey, mediaID, name, ownerID),
    ConnectionInfo (key, server, ts),
    ForwardID (forwardID),
    GeoInfo (lat, long),
    Host (Host),
    Message (attachments, forward, geo, peerID, text, tsMes),
    SettingsSet,
    UpdateID (UpdateID),
  )
import App.Utility (toByteString, tryGetResponse, tryGetResponseBotState)
import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import Data.Aeson (encode)
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    object,
    parseMaybe,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Network.HTTP.Simple
  ( Query,
    Request,
    defaultRequest,
    getResponseBody,
    setRequestHost,
    setRequestPath,
    setRequestPort,
    setRequestQueryString,
    setRequestSecure,
  )
import System.Exit (exitFailure)
import System.Random (Random (randomRIO))

main :: Logger.Handle IO -> IO ()
main logger = do
  config <- load logger
  let t = tokenVK config
  let group = groupVK config
  setSettings logger t group
  info <- getConnectionInfo logger t group
  let botHandle =
        Bot.Handle
          { Bot.getMessage = getMessage logger info,
            Bot.makeUpdateReq = makeUpdateReqFromMessage info,
            Bot.makeHelpReq = makeHelpReq t $ helpText config,
            Bot.makeRepeatReq = makeRepeatReq t,
            Bot.makeRepeatQuestionReq = makeRepeatQuestionReq t $ repeatText config,
            Bot.getText = MessageText . fromMaybe "" . text,
            Bot.getUserID = UserID . peerID,
            Bot.defaultRepeatNum = repeatNum config,
            Bot.markAsReadMes = markAsReadMes logger info t
          }
  _ <- runStateT (Bot.getUpdates botHandle Nothing) Bot.initialRepeatNumState
  return ()

versionAPI :: Double
versionAPI = 5.131

setSettings :: Logger.Handle IO -> Token -> GroupID -> IO ()
setSettings logger token groupID = do
  let req = makeSettingsReq token groupID
  resp <- tryGetResponse logger req
  let result = parseMaybe parseJSON $ getResponseBody resp :: Maybe SettingsSet
  case result of
    Just _ -> return ()
    _ -> do
      Logger.error logger "Could not set long poll settings"
      exitFailure

makeSettingsReq :: Token -> GroupID -> Request
makeSettingsReq (Token token) (GroupID groupID) =
  let path = Path "/method/groups.setLongPollSettings"
      host = Host "api.vk.com"
      param =
        [ ("group_id", Just groupID),
          ("access_token", Just token),
          ("enabled", Just $ toByteString (1 :: Integer)),
          ("message_new", Just $ toByteString (1 :: Integer)),
          ("v", Just $ toByteString versionAPI)
        ]
   in makeRequest path host param

getConnectionInfo :: Logger.Handle IO -> Token -> GroupID -> IO ConnectionInfo
getConnectionInfo logger token groupID = do
  let req = makeConnectionInfoReq token groupID
  resp <- tryGetResponse logger req
  Logger.debug logger ("Made request for connection info and got response:\n" ++ show resp)
  let info = parseMaybe parseJSON $ getResponseBody resp :: Maybe ConnectionInfo
  case info of
    Just x -> do
      Logger.info logger ("Got connection information:\n" ++ show x)
      return x
    Nothing -> do
      Logger.error logger "Connection information is not found"
      exitFailure

makeConnectionInfoReq :: Token -> GroupID -> Request
makeConnectionInfoReq (Token token) (GroupID groupID) =
  let path = Path "/method/groups.getLongPollServer"
      host = Host "api.vk.com"
      param =
        [ ("group_id", Just groupID),
          ("access_token", Just token),
          ("v", Just $ toByteString versionAPI)
        ]
   in makeRequest path host param

getMessage :: Logger.Handle IO -> ConnectionInfo -> Request -> BotState (Maybe Message)
getMessage logger info req = do
  response <- tryGetResponseBotState logger req
  liftIO $ Logger.debug logger ("Got response:\n" ++ show response)
  let mes = parseMaybe parseJSON $ getResponseBody response :: Maybe Message
  if isNothing mes
    then do
      let upd = parseMaybe parseJSON $ getResponseBody response :: Maybe UpdateID
      liftIO $ Logger.info logger ("Parsed response and got update id:\n" ++ show upd)
      case upd of
        Just x -> do
          newReq <- makeUpdateReq info x
          getMessage logger info newReq
        Nothing -> return Nothing
    else do
      liftIO $ Logger.info logger ("Parsed response and got message:\n" ++ show mes)
      return mes

makeRequest :: Path -> Host -> Query -> Request
makeRequest (Path path) (Host host) param =
  setRequestQueryString param $
    setRequestPath path $
      setRequestHost host $
        setRequestPort 443 $
          setRequestSecure True defaultRequest

makeUpdateReqFromMessage :: ConnectionInfo -> Maybe Message -> BotState Request
makeUpdateReqFromMessage info mes = do
  let ts' = case mes of
        Just m -> tsMes m
        Nothing -> ts info
  makeUpdateReq info (UpdateID ts')

makeUpdateReq :: ConnectionInfo -> UpdateID -> BotState Request
makeUpdateReq info (UpdateID updID) = do
  let path = Path $ Char8.pack (drop (length ("https://lp.vk.com" :: String)) (server info))
  let host = Host "lp.vk.com"
  let param =
        [ ("act", Just "a_check"),
          ("key", Just . Char8.pack $ key info),
          ("ts", Just $ Char8.pack updID),
          ("wait", Just "25")
        ]
  return $ makeRequest path host param

makeHelpReq :: Token -> HelpText -> Message -> BotState Request
makeHelpReq (Token token) (HelpText help) mes = do
  random <- getRandomNum
  let param =
        [ ("message", Just help),
          ("peer_id", Just $ toByteString $ peerID mes),
          ("random_id", Just $ toByteString random),
          ("access_token", Just token),
          ("v", Just $ toByteString versionAPI)
        ]
      path = Path "/method/messages.send"
      host = Host "api.vk.com"
  return $ makeRequest path host param

makeRepeatReq :: Token -> Message -> BotState Request
makeRepeatReq (Token token) mes = do
  random <- getRandomNum
  let botMessage = case text mes of
        Just x -> Just $ Encoding.encodeUtf8 $ Text.pack x
        Nothing -> Nothing
      botForwardMessage = case forward mes of
        Just x -> (Just . Char8.pack) (concatMap ((\y -> show y ++ ",") . forwardID) x)
        Nothing -> Nothing
      latitude = case geo mes of
        Just x -> Just . toByteString $ lat x
        Nothing -> Nothing
      longitude = case geo mes of
        Just x -> Just . toByteString $ long x
        Nothing -> Nothing
      param =
        [ ("message", botMessage),
          ("attachment", mediaToBS mes),
          ("forward_messages", botForwardMessage),
          ("peer_id", Just . toByteString $ peerID mes),
          ("random_id", Just $ toByteString random),
          ("access_token", Just token),
          ("sticker_id", stickerToBS mes),
          ("lat", latitude),
          ("long", longitude),
          ("v", Just $ toByteString versionAPI)
        ]
      path = Path "/method/messages.send"
      host = Host "api.vk.com"
  return $ makeRequest path host param

mediaToBS :: Message -> Maybe BS.ByteString
mediaToBS mes = case attachments mes of
  Just x -> Just . Char8.pack . mediaToString $ getMedia x
  Nothing -> Nothing
  where
    mediaToString :: [Attachment] -> String
    mediaToString [] = ""
    mediaToString [x] = makeString x
    mediaToString (x : xs) = makeString x ++ "," ++ mediaToString xs
    makeString :: Attachment -> String
    makeString x =
      let maybeKey = case accessKey x of
            Just k -> "_" ++ k
            Nothing -> ""
       in name x ++ show (ownerID x) ++ "_" ++ show (mediaID x) ++ maybeKey
    getMedia :: [Attachment] -> [Attachment]
    getMedia [] = []
    getMedia (m@Media {} : xs) = m : getMedia xs
    getMedia (_ : xs) = getMedia xs

stickerToBS :: Message -> Maybe BS.ByteString
stickerToBS mes = case attachments mes of
  Just x -> Just $ getSticker x
  Nothing -> Nothing
  where
    getSticker :: [Attachment] -> BS.ByteString
    getSticker [] = ""
    getSticker (Sticker s : _) = toByteString s
    getSticker (_ : xs) = getSticker xs

makeRepeatQuestionReq :: Token -> RepeatText -> Message -> RepeatNum -> BotState Request
makeRepeatQuestionReq (Token token) (RepeatText repText) mes (RepeatNum num) = do
  random <- getRandomNum
  let param =
        [ ("keyboard", Just $ BSL.toStrict $ encode buttons),
          ("message", Just $ repText `BS.append` toByteString num),
          ("peer_id", Just $ toByteString $ peerID mes),
          ("random_id", Just $ toByteString random),
          ("access_token", Just token),
          ("v", Just $ toByteString versionAPI)
        ]
      buttons =
        object
          [ "one_time" .= True,
            "buttons"
              .= [ [ object
                       [ "action"
                           .= object
                             [ "type" .= ("text" :: String),
                               "label" .= ("1" :: String)
                             ]
                       ],
                     object
                       [ "action"
                           .= object
                             [ "type" .= ("text" :: String),
                               "label" .= ("2" :: String)
                             ]
                       ],
                     object
                       [ "action"
                           .= object
                             [ "type" .= ("text" :: String),
                               "label" .= ("3" :: String)
                             ]
                       ],
                     object
                       [ "action"
                           .= object
                             [ "type" .= ("text" :: String),
                               "label" .= ("4" :: String)
                             ]
                       ],
                     object
                       [ "action"
                           .= object
                             [ "type" .= ("text" :: String),
                               "label" .= ("5" :: String)
                             ]
                       ]
                   ]
                 ]
          ]
      path = Path "/method/messages.send"
      host = Host "api.vk.com"
  return $ makeRequest path host param

markAsReadMes :: Logger.Handle IO -> ConnectionInfo -> Token -> Message -> BotState ()
markAsReadMes logger info (Token token) mes = do
  let param =
        [ ("message_ids", Just $ toByteString [tsMes mes]),
          ("peer_id", Just $ toByteString $ peerID mes),
          ("access_token", Just token),
          ("mark_conversation_as_read", Just "1"),
          ("v", Just $ toByteString versionAPI)
        ]
      path = Path "/method/messages.markAsRead"
      host = Host "api.vk.com"
  let req = makeRequest path host param
  _ <- getMessage logger info req
  let _ = Logger.info logger ("Marked as read message #" ++ tsMes mes)
  return ()

getRandomNum :: BotState Integer
getRandomNum = liftIO $ randomRIO (0, 100000000)
