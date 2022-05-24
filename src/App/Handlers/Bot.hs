{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Bot where

import App.Types.Bot (MessageText (..), RepeatNum (..), RepeatNumState (..), UserID, WaitingNewNum)
import Control.Monad.State (MonadState, forM_, get, modify, when)
import Data.Map (empty, insert, lookup)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import Prelude hiding (lookup, repeat)

data Handle m req mes = Handle
  { getMessage :: req -> m (Maybe mes),
    makeUpdateReq :: Maybe mes -> m req,
    makeHelpReq :: mes -> m req,
    makeRepeatReq :: mes -> m req,
    makeRepeatQuestionReq :: mes -> RepeatNum -> m req,
    getText :: mes -> MessageText,
    getUserID :: mes -> UserID,
    defaultRepeatNum :: RepeatNum,
    markAsReadMes :: mes -> m ()
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
  let mesText = getText handle mes
  let (MessageText text) = mesText
  waiting <- isWaitingNewNum userID
  if waiting
    then do
      let n = textToNum mesText
      let possibleValues = map (Just . RepeatNum) [1, 2, 3, 4, 5]
      when (n `elem` possibleValues) $ forM_ n (setRepeatNum userID)
      changeWaitingNewNum handle userID False
      markAsReadMes handle mes
      return $ Just mes
    else case text of
      "/help" -> sendHelp handle mes
      "/repeat" -> sendRepeatQuestion handle mes
      _ -> repeatMessage handle mes

sendHelp :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
sendHelp handle mes = do
  req <- makeHelpReq handle mes
  getMessage handle req

sendRepeatQuestion :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
sendRepeatQuestion handle mes = do
  let userID = getUserID handle mes
  num <- getRepeatNumber handle userID
  req <- makeRepeatQuestionReq handle mes num
  changeWaitingNewNum handle userID True
  getMessage handle req

changeWaitingNewNum :: MonadState RepeatNumState m => Handle m req mes -> UserID -> WaitingNewNum -> m ()
changeWaitingNewNum handle userID bool = do
  num <- getRepeatNumber handle userID
  modify $ \s -> s {repeatNums = insert userID (num, bool) $ repeatNums s}

isWaitingNewNum :: MonadState RepeatNumState m => UserID -> m WaitingNewNum
isWaitingNewNum userID = do
  state <- get
  let maybeValue = lookup userID $ repeatNums state
  case maybeValue of
    Just (_, waiting) -> return waiting
    Nothing -> return False

textToNum :: MessageText -> Maybe RepeatNum
textToNum (MessageText text) = RepeatNum <$> (readMaybe text :: Maybe Integer)

repeatMessage :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
repeatMessage handle mes = do
  let userID = getUserID handle mes
  (RepeatNum num) <- getRepeatNumber handle userID
  repeat num
  where
    repeat 1 = send
    repeat n = do
      _ <- send
      repeat (n -1)
    send = do
      req <- makeRepeatReq handle mes
      getMessage handle req

setRepeatNum :: MonadState RepeatNumState m => UserID -> RepeatNum -> m ()
setRepeatNum userId num =
  modify $ \s -> s {repeatNums = insert userId (num, False) $ repeatNums s}

getRepeatNumber :: MonadState RepeatNumState m => Handle m req mes -> UserID -> m RepeatNum
getRepeatNumber handle userId = do
  state <- get
  let maybeNum = lookup userId $ repeatNums state
  case maybeNum of
    Just (num, _) -> return num
    Nothing -> return $ defaultRepeatNum handle
