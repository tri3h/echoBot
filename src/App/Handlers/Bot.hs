{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.Bot where

import App.Types.Bot (MessageText (..), RepeatNum (..), RepeatNumState (..), UserID)
import Control.Monad (when)
import Control.Monad.State (MonadState, get, put)
import Data.Foldable (forM_)
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

getUpdate :: MonadState RepeatNumState m => Handle m req mes -> Maybe mes -> m mes
getUpdate handle mes = do
  req <- makeUpdateReq handle mes
  newMes <- getMessage handle req
  case newMes of
    Just m -> do
      answerMes <- chooseAnswer handle m
      let endMes = if isNothing answerMes then newMes else answerMes
      getUpdate handle endMes
    Nothing -> getUpdate handle newMes

chooseAnswer :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
chooseAnswer handle mes = do
  let (MessageText text) = getText handle mes
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
          let textNum = getText handle m
          let userID = getUserID handle m
          let n = textToNum textNum
          let possibleValues = map (Just . RepeatNum) [1, 2, 3, 4, 5]
          when (n `elem` possibleValues) $ forM_ n (setRepeatNum userID)
          markAsReadMes handle m
          return newMes
        Nothing -> getNum newMes

textToNum :: MessageText -> Maybe RepeatNum
textToNum (MessageText text) = RepeatNum <$> (readMaybe text :: Maybe Integer)

repeatMessage :: MonadState RepeatNumState m => Handle m req mes -> mes -> m (Maybe mes)
repeatMessage handle mes = do
  let userID = getUserID handle mes
  (RepeatNum num) <- getRepeatNum handle userID
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
setRepeatNum userId num = do
  state <- get
  put $ state {repeatNums = insert userId num $ repeatNums state}

getRepeatNum :: MonadState RepeatNumState m => Handle m req mes -> UserID -> m RepeatNum
getRepeatNum handle userId = do
  state <- get
  let maybeNum = lookup userId $ repeatNums state
  case maybeNum of
    Just x -> return x
    Nothing -> return $ defaultRepeatNum handle
