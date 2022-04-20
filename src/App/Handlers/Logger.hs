module App.Handlers.Logger (Handle (..), Verbosity (..), info, debug, warning, error, fromString) where

import Control.Monad (when)
import Prelude hiding (error, log)

data Handle m = Handle
  { writeLog :: String -> m (),
    verbosity :: Verbosity
  }

data Verbosity = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

log :: Monad m => Handle m -> Verbosity -> String -> m ()
log handle v str = when (v >= verbosity handle) $ writeLog handle $ toString v str

info :: Monad m => Handle m -> String -> m ()
info handle = log handle Info

debug :: Monad m => Handle m -> String -> m ()
debug handle = log handle Debug

warning :: Monad m => Handle m -> String -> m ()
warning handle = log handle Warning

error :: Monad m => Handle m -> String -> m ()
error handle = log handle Error

fromString :: String -> Maybe Verbosity
fromString "Info" = Just Info
fromString "Debug" = Just Debug
fromString "Warning" = Just Warning
fromString "Error" = Just Error
fromString _ = Nothing

toString :: Verbosity -> String -> String
toString v str = show v ++ ": " ++ str
