module Main where

import qualified App.Tg as Tg
import qualified App.Vk as Vk

main :: IO ()
main = do
  print "Type a number to choose bot (1 - Telegram, 2 - Vkontakte)"
  number <- getLine
  case number of
    "1" -> Tg.main
    "2" -> Vk.main
    _ -> do
      print "Incorrect number"
      main
