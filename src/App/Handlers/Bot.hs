{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Bot where

import Control.Monad.State (MonadState, forM_, get, put, when)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import Prelude hiding (lookup, repeat)

type UserID = Integer

type WaitingNewNum = Bool

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
  { repeatNums :: Map UserID (Integer, WaitingNewNum)
  }

initialRepeatNumState :: RepeatNumState
initialRepeatNumState =
  RepeatNumState
    { repeatNums = empty
    }

getUpdates :: MonadState RepeatNumState m => Handle m req mes -> Maybe mes -> m mes
getUpdates handle mes = do
  newMes <- getUpdate handle mes
  getUpdates handle newMes

getUpdate :: MonadState RepeatNumState m => Handle m req mes -> Maybe mes -> m (Maybe mes)
getUpdate handle mes = do
  req <- makeUpdateReq handle mes
  newMes <- getMessage handle req
  case newMes of
    Just m -> do
      answerMes <- chooseAnswer handle m
      return $ if isNothing answerMes then newMes else answerMes
    Nothing -> return Nothing

chooseAnswer :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
chooseAnswer handle mes = do
  let userID = getUserID handle mes
  let text = getText handle mes
  waiting <- isWaitingNewNum userID
  if waiting
    then do
      let n = textToNum text
      let possibleValues = map Just [1, 2, 3, 4, 5]
      when (n `elem` possibleValues) $ forM_ n (setRepeatNum userID)
      changeWaitingNewNum handle userID False
      markAsReadMes handle mes
      return $ Just mes
    else case text of
      "/help" -> sendHelp handle mes
      "/repeat" -> sendRepeatQuestion handle mes
      _ -> repeatMessage handle mes

textToNum :: String -> Maybe Integer
textToNum text = readMaybe text :: Maybe Integer

sendHelp :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
sendHelp handle mes = do
  req <- makeHelpReq handle mes
  getMessage handle req

sendRepeatQuestion :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
sendRepeatQuestion handle mes = do
  let userID = getUserID handle mes
  num <- getRepeatNum handle userID
  req <- makeRepeatQuestionReq handle mes num
  changeWaitingNewNum handle userID True
  getMessage handle req

changeWaitingNewNum :: MonadState RepeatNumState m => Handle m req mes -> UserID -> WaitingNewNum -> m ()
changeWaitingNewNum handle userID bool = do
  state <- get
  num <- getRepeatNum handle userID
  put $ state {repeatNums = insert userID (num, bool) $ repeatNums state}

isWaitingNewNum :: MonadState RepeatNumState m => UserID -> m WaitingNewNum
isWaitingNewNum userID = do
  state <- get
  let maybeValue = lookup userID $ repeatNums state
  case maybeValue of
    Just (_, waiting) -> return waiting
    Nothing -> return False

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
  put $ state {repeatNums = insert userId (num, False) $ repeatNums state}

getRepeatNum :: MonadState RepeatNumState m => Handle m req mes -> UserID -> m Integer
getRepeatNum handle userId = do
  state <- get
  let maybeNum = lookup userId $ repeatNums state
  case maybeNum of
    Just (num, _) -> return num
    Nothing -> return $ defaultRepeatNum handle
