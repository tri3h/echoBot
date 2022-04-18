{-# LANGUAGE OverloadedStrings #-}

module App.Types.Bot where

import App.Handlers.Logger (Verbosity (Info))
import qualified Data.ByteString as BS

newtype UserID = UserID Integer deriving (Eq, Ord)

newtype RepeatNum = RepeatNum Integer deriving (Eq, Ord, Show)

newtype MessageText = MessageText String

newtype Path = Path BS.ByteString

newtype Token = Token BS.ByteString

newtype HelpText = HelpText BS.ByteString

newtype RepeatText = RepeatText BS.ByteString

defaultLogVerbosity :: Verbosity
defaultLogVerbosity = Info

defaultHelpText :: BS.ByteString
defaultHelpText = "The most helpful text"

defaultRepeatNum :: Integer
defaultRepeatNum = 1

defaultRepeatText :: BS.ByteString
defaultRepeatText = "Echo sounds : "
