{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Bot where

type UserID = Integer

data Handle m req mes = Handle {
getMessage :: req -> m (Maybe mes),
makeUpdateReq :: Maybe mes -> m req,
makeHelpReq :: mes -> m req,
makeRepeatReq :: mes -> m req,
makeRepeatQuestionReq :: mes -> m req,
getText :: mes -> String,
getUserID :: mes -> UserID,
getRepeatNum :: UserID -> m Integer,
setRepeatNum :: UserID -> Integer -> m ()
}

getUpdate :: Monad m => Handle m req mes -> Maybe mes -> m mes
getUpdate handle mes = do
    req <- makeUpdateReq handle mes
    newMes <- getMessage handle req
    case newMes of
        Just m -> do
            chooseAnswer handle m
            getUpdate handle newMes
        Nothing -> getUpdate handle newMes

chooseAnswer :: Monad m => Handle m req mes -> mes -> m (Maybe mes)
chooseAnswer handle mes = do
    let text = getText handle mes
    case text of
        "/help" -> sendHelp handle mes
        "/repeat" -> changeRepeatNum handle mes
        _ -> repeatMessage handle mes 

sendHelp :: Monad m => Handle m req mes -> mes -> m (Maybe mes)
sendHelp handle mes = do
    req <- makeHelpReq handle mes
    getMessage handle req

changeRepeatNum :: Monad m => Handle m req mes -> mes -> m (Maybe mes)
changeRepeatNum handle mes = do
    req1 <- makeRepeatQuestionReq handle mes
    confirmMes1 <- getMessage handle req1
    req2 <- makeUpdateReq handle confirmMes1
    confirmMes2 <- getMessage handle req2
    numReq <- makeUpdateReq handle confirmMes2
    numMes <- getMessage handle numReq
    req3 <- makeUpdateReq handle numMes
    confirmMes3 <- getMessage handle req3
    case numMes of
        Just m -> do
            let num = getText handle m
            let userID = getUserID handle m
            setRepeatNum handle userID (read num :: Integer)
            return numMes
        Nothing -> return numMes

repeatMessage :: Monad m => Handle m req mes -> mes -> m (Maybe mes)
repeatMessage handle mes = do
    let userID = getUserID handle mes
    num <- getRepeatNum handle userID
    repeat handle num
    where repeat handle 1 = send
          repeat handle n = do
              send
              repeat handle (n-1)
          send = do
              req <- makeRepeatReq handle mes
              getMessage handle req
