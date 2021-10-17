{-# LANGUAGE OverloadedStrings #-}

module Tg where

import qualified Handlers.Tg as Handler

import qualified Data.Configurator as Config
import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified Network.HTTP.Simple as Net
import qualified Data.Text as Text
import qualified Data.List as List

main :: IO ()
main = do 
    config <- Config.load [Config.Required "bot.config"]
    let handle = Handler.Handle {
        Handler.getToken = do Config.require config "token",

        Handler.getHelpText = do Config.require config "help_text",

        Handler.getRepeatText = do Config.require config "repeat_text",

        Handler.getRepeatNum = \id -> do 
            let path = "repeat_times.id" ++ id
            userTimes <- Config.lookup config $ Text.pack path
            case userTimes of 
                Just x -> return x
                Nothing -> Config.require config "repeat_times.default",

        Handler.getResponse = \req -> do Net.httpJSON req,

        Handler.changeRepeatNum = \val id -> do
            let path = "bot.config"
                name = "id" ++ id
            handle <- IO.openFile path IO.ReadMode 
            contents <- IO.hGetContents handle
            let linedContents = lines contents
                indexUser = List.findIndex (List.isInfixOf name) linedContents
                indexDefault = List.findIndex (List.isInfixOf "default") linedContents
                newContents = case indexUser of 
                        Just x -> let (a, b) = splitAt x linedContents 
                                 in unlines $ a ++ [name ++ " = " ++ val] ++ tail b
                        Nothing -> case indexDefault of
                                    Just x -> let (a, b) = splitAt x linedContents 
                                        in unlines $ a ++ [name ++ " = " ++ val] ++ b
                                    Nothing -> contents
            (tempName, tempHandle) <- IO.openTempFile "." "temp"
            IO.hPutStr tempHandle newContents
            IO.hClose handle
            IO.hClose tempHandle
            Dir.removeFile path
            Dir.renameFile tempName path
            Config.reload config
    }
    Handler.getUpdates handle Nothing
