{-# LANGUAGE OverloadedStrings #-}

module App.Bot where

type UserID = Integer

data Handle m req mes = Handle {
getMessage :: req -> m mes,
makeUpdateReq :: Maybe mes -> m req,
makeHelpReq :: mes -> m req,
makeRepeatReq :: mes -> m req,
makeRepeatQuestionReq :: mes -> m req,
getText :: mes -> m String,
getUserID :: mes -> m UserID,
getRepeatNum :: UserID -> m Integer,
setRepeatNum :: UserID -> Integer -> m ()
}

getUpdate :: Monad m => Handle m req mes -> Maybe mes -> m mes
getUpdate handle mes = do
    req <- makeUpdateReq handle mes
    newMes <- getMessage handle req
    chooseAnswer handle newMes

chooseAnswer :: Monad m => Handle m req mes -> mes -> m mes
chooseAnswer handle mes = do
    text <- getText handle mes
    case text of
        "/help" -> sendHelp handle mes
        "/repeat" -> changeRepeatNum handle mes
        _ -> repeatMessage handle mes 

sendHelp :: Monad m => Handle m req mes -> mes -> m mes
sendHelp handle mes = do
    req <- makeHelpReq handle mes
    getMessage handle req

changeRepeatNum :: Monad m => Handle m req mes -> mes -> m mes
changeRepeatNum handle mes = do
    req <- makeRepeatQuestionReq handle mes
    numMes <- getMessage handle req
    num <- getText handle numMes
    userID <- getUserID handle numMes
    setRepeatNum handle userID (read num :: Integer)
    return numMes

repeatMessage :: Monad m => Handle m req mes -> mes -> m mes
repeatMessage handle mes = do
    userID <- getUserID handle mes
    num <- getRepeatNum handle userID
    repeat handle num
    where repeat handle 1 = send
          repeat handle n = do
              send
              repeat handle (n-1)
          send = do
              req <- makeRepeatReq handle mes
              getMessage handle req
