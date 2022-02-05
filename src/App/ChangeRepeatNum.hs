module App.ChangeRepeatNum (changeRepeatNum) where

import qualified App.Handlers.ChangeRepeatNum as H
import qualified System.IO as IO
import qualified System.Directory as Dir
import Prelude hiding (writeFile, readFile)

changeRepeatNum :: FilePath -> String -> String -> IO ()
changeRepeatNum = H.changeRepeatNum handle
    where handle = H.Handle {
        H.readFile = readFile,
        H.writeFile = writeFile
    }

readFile :: FilePath -> IO String
readFile path = do
            h <- IO.openFile path IO.ReadMode
            contents <- IO.hGetContents h
            IO.hClose h
            return contents

writeFile :: FilePath -> String -> IO ()
writeFile path newContents = do
           (tempName, tempHandle) <- IO.openTempFile "." "temp"
           IO.hPutStr tempHandle newContents
           IO.hClose tempHandle
           Dir.removeFile path
           Dir.renameFile tempName path