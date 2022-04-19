{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Tg where

import qualified App.Handlers.Bot as Bot
import qualified App.Handlers.Logger as Logger
import App.Types.Bot (BotState)
import App.Types.Tg
  ( Message (chatID, fromChatID, messageID, text, updateID),
  )
import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Value (Null),
    object,
    parseMaybe,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Configurator as Config
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
  ( Query,
    Request,
    defaultRequest,
    getResponseBody,
    httpJSON,
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
  config <- Config.load [Config.Required "Configs/TG.config"]
  token <- Config.require config "token"
  helpText <- Config.require config "help_text"
  repeatText <- Config.require config "repeat_text"
  defaultNum <- Config.require config "repeat_times.default"
  maybeLogVerbosity <- Config.require config "log_verbosity"
  logVerbosity <- maybe exitFailure return (Logger.fromString maybeLogVerbosity)
  let loggerHandle =
        Logger.Handle
          { Logger.verbosity = logVerbosity,
            Logger.writeLog = liftIO . putStrLn
          }
  let botHandle =
        Bot.Handle
          { Bot.getMessage = getMessage loggerHandle,
            Bot.makeUpdateReq = makeUpdateReq token,
            Bot.makeHelpReq = makeHelpReq token helpText,
            Bot.makeRepeatReq = makeRepeatReq token,
            Bot.makeRepeatQuestionReq = makeRepeatQuestionReq token repeatText,
            Bot.getText = fromMaybe "" . text,
            Bot.getUserID = chatID,
            Bot.defaultRepeatNum = defaultNum,
            Bot.markAsReadMes = markAsReadMes loggerHandle token
          }
  _ <- runStateT (Bot.getUpdate botHandle Nothing) Bot.initialRepeatNumState
  return ()

makeRequest :: BS.ByteString -> Query -> Value -> Request
makeRequest path params json =
  setRequestBodyJSON json $
    setRequestQueryString params $
      setRequestPath path $
        setRequestHost "api.telegram.org" $
          setRequestPort 443 $
            setRequestSecure True defaultRequest

getMessage :: Logger.Handle BotState -> Request -> BotState (Maybe Message)
getMessage logger req = do
  response <- httpJSON req
  Logger.debug logger ("Got response:\n" ++ show response)
  let mes = parseMaybe parseJSON $ getResponseBody response :: Maybe Message
  Logger.info logger ("Parsed response and got message:\n" ++ show mes)
  return mes

makeUpdateReq :: BS.ByteString -> Maybe Message -> BotState Request
makeUpdateReq token mes = do
  let params =
        [ ("limit", Just "1"),
          ("timeout", Just "10"),
          ("offset", getOffset mes)
        ]
      path = "/bot" `BS.append` token `BS.append` "/getUpdates"
  return $ makeRequest path params Null
  where
    getOffset :: Maybe Message -> Maybe BS.ByteString
    getOffset Nothing = Nothing
    getOffset (Just m) = Just . toByteString $ updateID m + 1

makeHelpReq :: BS.ByteString -> BS.ByteString -> Message -> BotState Request
makeHelpReq token helpText mes = do
  let params =
        [ ("chat_id", Just . toByteString $ chatID mes),
          ("text", Just helpText)
        ]
      path = "/bot" `BS.append` token `BS.append` "/sendMessage"
  return $ makeRequest path params Null

makeRepeatReq :: BS.ByteString -> Message -> BotState Request
makeRepeatReq token mes = do
  let param =
        [ ("chat_id", chatID mes),
          ("from_chat_id", fromChatID mes),
          ("message_id", messageID mes)
        ]
      params = fmap (\(a, b) -> (a, Just $ toByteString b)) param
      path = "/bot" `BS.append` token `BS.append` "/copyMessage"
  return $ makeRequest path params Null

makeRepeatQuestionReq :: BS.ByteString -> BS.ByteString -> Message -> Integer -> BotState Request
makeRepeatQuestionReq token repeatText mes num = do
  let params =
        [ ("chat_id", Just . toByteString $ chatID mes),
          ("text", Just $ repeatText `BS.append` toByteString num)
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
      path = "/bot" `BS.append` token `BS.append` "/sendMessage"
  return $ makeRequest path params buttons

markAsReadMes :: Logger.Handle BotState -> BS.ByteString -> Message -> BotState ()
markAsReadMes logger token mes = do
  req <- makeUpdateReq token (Just mes)
  _ <- getMessage logger req
  return ()

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x
