module App.Types.Bot where

import qualified App.Handlers.Bot as Bot
import Control.Monad.State (StateT)

type BotState = StateT Bot.RepeatNumState IO

