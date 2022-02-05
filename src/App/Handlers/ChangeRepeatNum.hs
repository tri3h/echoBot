module App.Handlers.ChangeRepeatNum where

import Prelude hiding (readFile, writeFile)
import qualified Data.List as List

data Handle m = Handle {
    readFile :: String -> m String,
    writeFile :: String -> String -> m ()
}

changeRepeatNum :: Monad m => Handle m -> FilePath -> String -> String -> m ()
changeRepeatNum handle path val id = do
           let name = "id" ++ id
           contents <- readFile handle path
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
           writeFile handle path newContents