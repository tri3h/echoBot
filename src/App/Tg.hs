{-# LANGUAGE OverloadedStrings #-}

module App.Tg where

import App.Config (load)
import qualified App.Handlers.Bot as Bot
import qualified App.Handlers.Logger as Logger
import App.Logger (make)
import App.Types.Bot
  ( BotState,
    MessageText (MessageText),
    Path (..),
    RepeatNum (..),
    UserID (UserID),
  )
import App.Types.Config
  ( Config (tokenTG),
    HelpText (HelpText),
    RepeatText (RepeatText),
    Token (Token),
    helpText,
    repeatNum,
    repeatText,
  )
import App.Types.Tg
  ( Message (chatID, fromChatID, messageID, text, updateID),
  )
import App.Utility (toByteString, tryGetResponseBotState)
import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Value (Null),
    object,
    parseMaybe,
  )
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
  ( Query,
    Request,
    defaultRequest,
    getResponseBody,
    getResponseStatusCode,
    setRequestBodyJSON,
    setRequestHost,
    setRequestPath,
    setRequestPort,
    setRequestQueryString,
    setRequestSecure,
  )
import System.Exit (exitFailure)

main :: IO ()
main = do
  logger <- make
  config <- load logger
  let t = tokenTG config
  let botHandle =
        Bot.Handle
          { Bot.getMessage = getMessage logger,
            Bot.makeUpdateReq = makeUpdateReq t,
            Bot.makeHelpReq = makeHelpReq t $ helpText config,
            Bot.makeRepeatReq = makeRepeatReq t,
            Bot.makeRepeatQuestionReq = makeRepeatQuestionReq t $ repeatText config,
            Bot.getText = MessageText . fromMaybe "" . text,
            Bot.getUserID = UserID . chatID,
            Bot.defaultRepeatNum = repeatNum config,
            Bot.markAsReadMes = markAsReadMes logger t
          }
  _ <- runStateT (Bot.getUpdates botHandle Nothing) Bot.initialRepeatNumState
  return ()

makeRequest :: Path -> Query -> Value -> Request
makeRequest (Path path) params json =
  setRequestBodyJSON json $
    setRequestQueryString params $
      setRequestPath path $
        setRequestHost "api.telegram.org" $
          setRequestPort 443 $
            setRequestSecure True defaultRequest

getMessage :: Logger.Handle IO -> Request -> BotState (Maybe Message)
getMessage logger req = do
  response <- tryGetResponseBotState logger req
  let statusCode = getResponseStatusCode response
  if statusCode == 404
    then do
      liftIO $ Logger.error logger "Invalid token"
      liftIO exitFailure
    else do
      liftIO $ Logger.debug logger ("Got response:\n" ++ show response)
      let mes = parseMaybe parseJSON $ getResponseBody response :: Maybe Message
      liftIO $ Logger.info logger ("Parsed response and got message:\n" ++ show mes)
      return mes

makeUpdateReq :: Token -> Maybe Message -> BotState Request
makeUpdateReq (Token token) mes = do
  let params =
        [ ("limit", Just "1"),
          ("timeout", Just "10"),
          ("offset", getOffset mes)
        ]
      path = Path $ "/bot" `BS.append` token `BS.append` "/getUpdates"
  return $ makeRequest path params Null
  where
    getOffset :: Maybe Message -> Maybe BS.ByteString
    getOffset Nothing = Nothing
    getOffset (Just m) = Just . toByteString $ updateID m + 1

makeHelpReq :: Token -> HelpText -> Message -> BotState Request
makeHelpReq (Token token) (HelpText help) mes = do
  let params =
        [ ("chat_id", Just . toByteString $ chatID mes),
          ("text", Just help)
        ]
      path = Path $ "/bot" `BS.append` token `BS.append` "/sendMessage"
  return $ makeRequest path params Null

makeRepeatReq :: Token -> Message -> BotState Request
makeRepeatReq (Token token) mes = do
  let param =
        [ ("chat_id", chatID mes),
          ("from_chat_id", fromChatID mes),
          ("message_id", messageID mes)
        ]
      params = fmap (\(a, b) -> (a, Just $ toByteString b)) param
      path = Path $ "/bot" `BS.append` token `BS.append` "/copyMessage"
  return $ makeRequest path params Null

makeRepeatQuestionReq :: Token -> RepeatText -> Message -> RepeatNum -> BotState Request
makeRepeatQuestionReq (Token token) (RepeatText repText) mes (RepeatNum num) = do
  let params =
        [ ("chat_id", Just . toByteString $ chatID mes),
          ("text", Just $ repText `BS.append` toByteString num)
        ]
      buttons =
        object
          [ "reply_markup"
              .= object
                [ "keyboard" .= [["1", "2", "3", "4", "5" :: String]],
                  "resize_keyboard" .= True,
                  "one_time_keyboard" .= True
                ]
          ]
      path = Path $ "/bot" `BS.append` token `BS.append` "/sendMessage"
  return $ makeRequest path params buttons

markAsReadMes :: Logger.Handle IO -> Token -> Message -> BotState ()
markAsReadMes logger token mes = do
  req <- makeUpdateReq token (Just mes)
  _ <- getMessage logger req
  return ()
