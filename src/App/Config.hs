{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Config where

import qualified App.Handlers.Logger as Logger
import App.Types.Bot (RepeatNum (RepeatNum, getRepeatNum))
import App.Types.Config
  ( Config (..),
    GroupID (GroupID),
    HelpText (HelpText, getHelpText),
    RepeatText (RepeatText, getRepeatText),
    Token (Token),
    defaultHelpText,
    defaultRepeatNum,
    defaultRepeatText,
  )
import qualified Data.Configurator as Config
import System.Exit (exitFailure)

load :: Logger.Handle IO -> IO Config
load logger = do
  config <- Config.load [Config.Required "Configs/Bot.config"]
  helpText <- HelpText <$> Config.lookupDefault (getHelpText defaultHelpText) config "help_text"
  repeatText <- RepeatText <$> Config.lookupDefault (getRepeatText defaultRepeatText) config "repeat_text"
  repeatNum <- RepeatNum <$> Config.lookupDefault (getRepeatNum defaultRepeatNum) config "default_repeat_num"
  maybeTokenVK <- Config.lookup config "VK.token"
  tokenVK <- case maybeTokenVK of
    Just x -> return $ Token x
    Nothing -> do
      Logger.error logger "Token for VK has invalid format"
      exitFailure
  maybeGroupVK <- Config.lookup config "VK.group_id"
  groupVK <- case maybeGroupVK of
    Just x -> return $ GroupID x
    Nothing -> do
      Logger.error logger "Group id has invalid format"
      exitFailure
  maybeTokenTG <- Config.lookup config "TG.token"
  tokenTG <- case maybeTokenTG of
    Just x -> return $ Token x
    Nothing -> do
      Logger.error logger "Token for TG has invalid format"
      exitFailure
  return Config {..}
