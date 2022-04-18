module App.Utility where

import qualified App.Handlers.Logger as Logger
import Control.Exception (catch)
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Network.HTTP.Client.Conduit
  ( HttpException (HttpExceptionRequest),
    HttpExceptionContent (ConnectionFailure),
    Response,
  )
import Network.HTTP.Conduit (Request)
import Network.HTTP.Simple (httpJSON)
import System.Exit (exitFailure)

toByteString :: Show a => a -> BS.ByteString
toByteString x = Char8.pack $ show x

tryGetResponse :: Logger.Handle IO -> Request -> IO (Response Value)
tryGetResponse logger req =
  catch
    (httpJSON req)
    ( \e -> do
        case e of
          HttpExceptionRequest _ content -> case content of
            ConnectionFailure _ -> Logger.error logger "No connection"
            _ -> Logger.error logger "Unknown error"
          _ -> Logger.error logger "Unknown error"
        exitFailure
    )
