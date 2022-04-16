module App.Utility where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x
