module Main where

import qualified App.Handlers.Logger as Logger
import App.Logger (make)
import qualified App.Tg as Tg
import qualified App.Vk as Vk
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  logger <- make
  case args of
    "tg" : _ -> Tg.main logger
    "vk" : _ -> Vk.main logger
    _ -> Logger.error logger "No bot selected"
