{-# LANGUAGE OverloadedStrings #-}

module App.Types.Config where

import App.Types.Bot (RepeatNum (RepeatNum))
import qualified Data.ByteString as BS

newtype Token = Token {getToken :: BS.ByteString}

newtype HelpText = HelpText {getHelpText :: BS.ByteString}

newtype RepeatText = RepeatText {getRepeatText :: BS.ByteString}

newtype GroupID = GroupID {getGroupID :: BS.ByteString}

data Config = Config
  { helpText :: HelpText,
    repeatText :: RepeatText,
    repeatNum :: RepeatNum,
    tokenTG :: Token,
    tokenVK :: Token,
    groupVK :: GroupID
  }

defaultHelpText :: HelpText
defaultHelpText = HelpText "The most helpful text"

defaultRepeatNum :: RepeatNum
defaultRepeatNum = RepeatNum 1

defaultRepeatText :: RepeatText
defaultRepeatText = RepeatText "Echo sounds : "
