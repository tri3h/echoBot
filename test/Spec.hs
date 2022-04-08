{-# LANGUAGE LambdaCase #-}

import qualified App.Handlers.Bot as H
import Data.Functor.Identity (Identity)
import Test.Hspec

handle :: H.Handle Identity String String
handle =
  H.Handle
    { H.getMessage = return . Just,
      H.makeUpdateReq = \case
        Just m -> return m
        Nothing -> return "",
      H.makeHelpReq = return,
      H.makeRepeatReq = return,
      H.makeRepeatQuestionReq = return,
      H.getText = id,
      H.getUserID = const 0,
      H.getRepeatNum = \_ -> return 1,
      H.setRepeatNum = \_ _ -> return ()
    }

main :: IO ()
main = hspec $ do
  describe "Testing choose answer" $ do
    it "Should send help text" $ do
      let handleCase =
            handle
              { H.makeHelpReq = \mes -> return $ "This is help text for " ++ mes
              }
          result = H.chooseAnswer handleCase "/help"
      result `shouldBe` return (Just "This is help text for /help")
    it "Should send repeat number question" $ do
      let handleCase =
            handle
              { H.makeRepeatQuestionReq = \mes -> return $ "This is repeat number question for " ++ mes
              }
          result = H.chooseAnswer handleCase "/repeat"
      result `shouldBe` return (Just "This is repeat number question for /repeat")
    it "Should send same message" $ do
      let result = H.chooseAnswer handle "This is message"
      result `shouldBe` return (Just "This is message")
  describe "Testing send help" $
    it "Should send help text" $ do
      let handleCase =
            handle
              { H.makeHelpReq = \mes -> return $ "This is help text for " ++ mes
              }
          result = H.sendHelp handleCase "message"
      result `shouldBe` return (Just "This is help text for message")
  describe "Testing change repeat number" $
    it "Should change repeat number" $ do
      let handleCase =
            handle
              { H.getMessage = \_ -> return $ Just "5"
              }
          result = H.changeRepeatNum handleCase "Message"
      result `shouldBe` return (Just "5")
  describe "Testing repeat message" $
    it "Should send same message" $ do
      let result = H.repeatMessage handle "This is message"
      result `shouldBe` return (Just "This is message")
