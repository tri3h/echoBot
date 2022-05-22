{-# LANGUAGE LambdaCase #-}

import App.Handlers.Bot (initialRepeatNumState)
import qualified App.Handlers.Bot as H
import App.Types.Bot
  ( MessageText (MessageText),
    RepeatNum (..),
    RepeatNumState (RepeatNumState),
    UserID (UserID),
  )
import Control.Monad.State (StateT, evalStateT)
import Data.Functor.Identity (Identity)
import Test.Hspec (describe, hspec, it, shouldBe)

handle :: H.Handle (StateT RepeatNumState Identity) String String
handle =
  H.Handle
    { H.getMessage = return . Just,
      H.makeUpdateReq = \case
        Just m -> return m
        Nothing -> return "",
      H.makeHelpReq = return,
      H.makeRepeatReq = return,
      H.makeRepeatQuestionReq = \mes (RepeatNum n) -> return (mes ++ show n),
      H.getText = MessageText,
      H.getUserID = UserID . const 0,
      H.defaultRepeatNum = RepeatNum 1,
      H.markAsReadMes = \_ -> return ()
    }

makeResult :: Monad m => StateT RepeatNumState m a -> m a
makeResult state = evalStateT state initialRepeatNumState

main :: IO ()
main = hspec $ do
  describe "Testing choose answer" $ do
    it "Should send help text" $ do
      let handleCase =
            handle
              { H.makeHelpReq = \mes -> return $ "This is help text for " ++ mes
              }
          result = makeResult $ H.chooseAnswer handleCase "/help"
      result `shouldBe` return (Just "This is help text for /help")
    it "Should send repeat number question" $ do
      let handleCase =
            handle
              { H.getMessage = \_ -> return $ Just "2"
              }
      let result = makeResult $ H.chooseAnswer handleCase "/repeat"
      result `shouldBe` return (Just "2")
    it "Should send same message" $ do
      let result = makeResult $ H.chooseAnswer handle "This is message"
      result `shouldBe` return (Just "This is message")
  describe "Testing send help" $
    it "Should send help text" $ do
      let handleCase =
            handle
              { H.makeHelpReq = \mes -> return $ "This is help text for " ++ mes
              }
          result = makeResult $ H.sendHelp handleCase "message"
      result `shouldBe` return (Just "This is help text for message")
  describe "Testing change repeat number" $ do
    it "Should get default repeat number" $ do
      let result = makeResult $ H.getRepeatNumber handle (UserID 2)
      result `shouldBe` return (RepeatNum 1)
    it "Should set repeat number for the user" $ do
      let result = makeResult $ H.setRepeatNum (UserID 1) (RepeatNum 5) >> H.getRepeatNumber handle (UserID 1)
      result `shouldBe` return (RepeatNum 5)
  describe "Testing repeat message" $
    it "Should send same message" $ do
      let result = makeResult $ H.repeatMessage handle "This is message"
      result `shouldBe` return (Just "This is message")
