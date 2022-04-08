{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Types.Tg where

import Data.Aeson (FromJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (FromJSON (parseJSON))

data Message = Message
  { updateID :: Integer,
    chatID :: Integer,
    fromChatID :: Integer,
    messageID :: Integer,
    text :: Maybe String
  }
  deriving (Show)

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
    return Message {..}
