{-# LANGUAGE OverloadedStrings #-}

module App.Logger where

import qualified App.Handlers.Logger as Logger
import App.Types.Bot (defaultLogVerbosity)
import qualified Data.Configurator as Config
import Data.Maybe (fromMaybe)

make :: IO (Logger.Handle IO)
make = do
  config <- Config.load [Config.Required "Configs/Bot.config"]
  maybeLogVerbosity <- Config.lookup config "log_verbosity"
  let logVerbosity = case maybeLogVerbosity of
        Just x -> fromMaybe defaultLogVerbosity $ Logger.fromString x
        Nothing -> defaultLogVerbosity
  return $
    Logger.Handle
      { Logger.verbosity = logVerbosity,
        Logger.writeLog = putStrLn
      }
