module App.Handlers.Logger(Handle(..), Verbosity(..), debug, warning, error) where

import Prelude hiding (log, error)

data Handle m = Handle {
    writeLog :: String -> m (),
    verbosity :: Verbosity
}

data Verbosity = Debug | Warning | Error deriving (Eq, Ord, Show)

log :: Monad m => Handle m -> Verbosity -> String -> m ()
log handle v str = if v >= (verbosity handle)
                        then writeLog handle $ toString v str
                        else return ()


debug :: Monad m => Handle m -> String -> m ()
debug handle = log handle Debug

warning :: Monad m => Handle m -> String -> m ()
warning handle = log handle Warning

error :: Monad m => Handle m -> String -> m ()
error handle = log handle Error

toString :: Verbosity -> String -> String
toString v str = show v ++ ": " ++ str