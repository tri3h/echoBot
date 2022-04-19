{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Bot where

import Control.Monad.State (MonadState, get, put)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (isNothing)
import Prelude hiding (lookup, repeat)

type UserID = Integer

data Handle m req mes = Handle
  { getMessage :: req -> m (Maybe mes),
    makeUpdateReq :: Maybe mes -> m req,
    makeHelpReq :: mes -> m req,
    makeRepeatReq :: mes -> m req,
    makeRepeatQuestionReq :: mes -> Integer -> m req,
    getText :: mes -> String,
    getUserID :: mes -> UserID,
    defaultRepeatNum :: Integer,
    markAsReadMes :: mes -> m ()
  }

newtype RepeatNumState = RepeatNumState
  { repeatNums :: Map UserID Integer
  }

initialRepeatNumState :: RepeatNumState
initialRepeatNumState =
  RepeatNumState
    { repeatNums = empty
    }

getUpdate :: MonadState RepeatNumState m => Handle m req mes -> Maybe mes -> m mes
getUpdate handle mes = do
  req <- makeUpdateReq handle mes
  newMes <- getMessage handle req
  case newMes of
    Just m ->
      if null $ getText handle m
        then getUpdate handle newMes
        else do
          answerMes <- chooseAnswer handle m
          let lastMes = if isNothing answerMes then newMes else answerMes
          getUpdate handle lastMes
    Nothing -> getUpdate handle newMes

chooseAnswer :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
chooseAnswer handle mes = do
  let text = getText handle mes
  case text of
    "/help" -> sendHelp handle mes
    "/repeat" -> changeRepeatNum handle mes
    _ -> repeatMessage handle mes

sendHelp :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
sendHelp handle mes = do
  req <- makeHelpReq handle mes
  getMessage handle req

changeRepeatNum :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
changeRepeatNum handle iniMes = do
  let userId = getUserID handle iniMes
  num <- getRepeatNum handle userId
  req <- makeRepeatQuestionReq handle iniMes num
  _ <- getMessage handle req
  getNum (Just iniMes)
  where
    getNum mes = do
      req <- makeUpdateReq handle mes
      newMes <- getMessage handle req
      case newMes of
        Just m -> do
          let num = getText handle m
          let userID = getUserID handle m
          setRepeatNum userID (read num :: Integer)
          markAsReadMes handle m
          return newMes
        Nothing -> getNum newMes

repeatMessage :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
repeatMessage handle mes = do
  let userID = getUserID handle mes
  num <- getRepeatNum handle userID
  repeat num
  where
    repeat 1 = send
    repeat n = do
      _ <- send
      repeat (n -1)
    send = do
      req <- makeRepeatReq handle mes
      getMessage handle req

setRepeatNum :: MonadState RepeatNumState m => UserID -> Integer -> m ()
setRepeatNum userId num = do
  state <- get
  put $ state {repeatNums = insert userId num $ repeatNums state}

getRepeatNum :: MonadState RepeatNumState m => Handle m req mes -> UserID -> m Integer
getRepeatNum handle userId = do
  state <- get
  let maybeNum = lookup userId $ repeatNums state
  case maybeNum of
    Just x -> return x
    Nothing -> return $ defaultRepeatNum handle
