{-# LANGUAGE OverloadedStrings #-}

module App.Handle where

{-
data Config = Config {
path :: FilePath,
repeatText :: String,
helpText :: String
}
-}

data Handle m a b = Handle {
--hConfig :: Config,
getMessage :: a -> m b,
getText :: b -> String,
makeHelpReq :: b -> m a,
makeRepeatNumReq :: b -> m a,
makeRepeatReq :: b -> m a,
makeMessageReq :: Maybe b -> m a,
setRepeatNum :: b -> m b,
getRepeatNum :: Maybe b -> m Int
}

getUpdate :: Monad m => Handle m a b -> Maybe b -> m b
getUpdate handle mes = do
    req <- makeMessageReq handle mes
    newMes <- getMessage handle req
    chooseAnswer handle newMes

chooseAnswer :: Monad m => Handle m a b -> b -> m b
chooseAnswer handle mes = case getText handle mes of
    "/help" -> sendHelp handle mes
    "/repeat" -> changeRepeatNum handle mes
    _ -> repeatMessage handle mes 

sendHelp :: Monad m => Handle m a b -> b -> m b
sendHelp handle mes = do
    req <- makeHelpReq handle mes
    getMessage handle req

changeRepeatNum :: Monad m => Handle m a b -> b -> m b
changeRepeatNum handle mes = do
    req <- makeRepeatNumReq handle mes
    numMes <- getMessage handle req
    setRepeatNum handle numMes

repeatMessage :: Monad m => Handle m a b -> b -> m b
repeatMessage handle mes = do
    num <- getRepeatNum handle (Just mes)
    req <- makeRepeatReq handle mes
    send handle req num
    where send handle req 1 = getMessage handle req
          send handle req n = do
              getMessage handle req
              send handle req (n-1)