{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Bot where

import Control.Monad.State (StateT, get, lift, put)
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

getUpdate :: Monad m => Handle m req mes -> Maybe mes -> StateT RepeatNumState m mes
getUpdate handle mes = do
  req <- lift $ makeUpdateReq handle mes
  newMes <- lift $ getMessage handle req
  case newMes of
    Just m -> do
      answerMes <- chooseAnswer handle m
      let endMes = if isNothing answerMes then newMes else answerMes
      getUpdate handle endMes
    Nothing -> getUpdate handle newMes

chooseAnswer :: Monad m => Handle m req mes -> mes -> StateT RepeatNumState m (Maybe mes)
chooseAnswer handle mes = do
  let text = getText handle mes
  case text of
    "/help" -> sendHelp handle mes
    "/repeat" -> changeRepeatNum handle mes
    _ -> repeatMessage handle mes

sendHelp :: Monad m => Handle m req mes -> mes -> StateT RepeatNumState m (Maybe mes)
sendHelp handle mes = do
  req <- lift $ makeHelpReq handle mes
  lift $ getMessage handle req

changeRepeatNum :: Monad m => Handle m req mes -> mes -> StateT RepeatNumState m (Maybe mes)
changeRepeatNum handle iniMes = do
  let userId = getUserID handle iniMes
  num <- getRepeatNum handle userId
  req <- lift $ makeRepeatQuestionReq handle iniMes num
  _ <- lift $ getMessage handle req
  getNum (Just iniMes)
  where
    getNum mes = do
      req <- lift $ makeUpdateReq handle mes
      newMes <- lift $ getMessage handle req
      case newMes of
        Just m -> do
          let num = getText handle m
          let userID = getUserID handle m
          setRepeatNum userID (read num :: Integer)
          lift $ markAsReadMes handle m
          return newMes
        Nothing -> getNum newMes

repeatMessage :: Monad m => Handle m req mes -> mes -> StateT RepeatNumState m (Maybe mes)
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
      req <- lift $ makeRepeatReq handle mes
      lift $ getMessage handle req

setRepeatNum :: Monad m => UserID -> Integer -> StateT RepeatNumState m ()
setRepeatNum userId num = do
  state <- get
  put $ state {repeatNums = insert userId num $ repeatNums state}

getRepeatNum :: Monad m => Handle m req mes -> UserID -> StateT RepeatNumState m Integer
getRepeatNum handle userId = do
  state <- get
  let maybeNum = lookup userId $ repeatNums state
  case maybeNum of
    Just x -> return x
    Nothing -> return $ defaultRepeatNum handle
