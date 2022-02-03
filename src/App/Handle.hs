{-# LANGUAGE OverloadedStrings #-}

module App.Handle where

data Config = Config {
path :: FilePath,
repeatText :: String,
helpText :: String
}

data Handle m a b = Handle {
hConfig :: Config,
getMessage :: a -> m b,
makeHelpReq :: b -> a,
makeRepeatNumReq :: b -> a,
makeRepeatReq :: b -> a,
makeMessageReq :: Maybe b -> a,
changeRepeatNum :: b -> m (),
getRepeatNum :: Maybe b -> Int
}

chooseAnswer :: Monad m => Handle m a b -> b -> String -> m ()
chooseAnswer handle mes s = case s of
    "/help" -> sendHelp handle mes
    "/repeat" -> setNewRepeatNum handle mes
    _ -> repeatMessage handle mes 

setNewRepeatNum :: Monad m => Handle m a b -> b -> m ()
setNewRepeatNum handle mes = do
    let req = makeRepeatNumReq handle mes
    numMes <- getMessage handle req
    changeRepeatNum handle numMes

sendHelp :: Monad m => Handle m a b -> b -> m ()
sendHelp handle mes = do
    let req = makeHelpReq handle mes
    getMessage handle req
    return ()

repeatMessage :: Monad m => Handle m a b -> b -> m ()
repeatMessage handle mes = do
    let num = getRepeatNum handle (Just mes)
        req = makeRepeatReq handle mes
    send handle req num
    where send :: Monad m => Handle m a b -> a -> Int -> m ()
          send _ _ 0 = return ()
          send handle req n = do
              getMessage handle req
              send handle req (n-1)