module App.Types.Bot where

import App.Handlers.Logger (Verbosity (Info))
import Control.Monad.State (StateT)
import qualified Data.ByteString as BS
import Data.Map (Map)

newtype UserID = UserID Integer deriving (Eq, Ord)

newtype RepeatNum = RepeatNum {getRepeatNum :: Integer} deriving (Eq, Ord, Show)

newtype MessageText = MessageText String

newtype Path = Path BS.ByteString

newtype RepeatNumState = RepeatNumState
  { repeatNums :: Map UserID (RepeatNum, WaitingNewNum)
  }

type WaitingNewNum = Bool

type BotState = StateT RepeatNumState IO

defaultLogVerbosity :: Verbosity
defaultLogVerbosity = Info
