module App.Types.Bot where

import qualified Data.ByteString as BS

newtype UserID = UserID Integer deriving (Eq, Ord)

newtype RepeatNum = RepeatNum Integer deriving (Eq, Ord, Show)

newtype MessageText = MessageText String

newtype Path = Path BS.ByteString

newtype Token = Token BS.ByteString

newtype HelpText = HelpText BS.ByteString

newtype RepeatText = RepeatText BS.ByteString
