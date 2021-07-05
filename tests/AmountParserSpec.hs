{-# LANGUAGE OverloadedStrings #-}

module AmountParserSpec (spec) where

import           Test.Hspec

import           Control.Monad.Trans.State.Strict
import           Data.Either (either, isLeft)
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Hledger as HL
import           Text.Megaparsec

import           AmountParser

spec :: Spec
spec = describe "parseAmount" $ do
  it "parses single amount" $
    parseAmount HL.nulljournal "42" `shouldBe` Right (amount "42")

  it "parses a positive number" $
    parseAmount HL.nulljournal "+42" `shouldBe` Right (amount "42")

  it "parses a negative number" $
    parseAmount HL.nulljournal "-42" `shouldBe` Right (amount "-42")

  it "parses a simple sum" $
    parseAmount HL.nulljournal "23 + 23 + 42 + 1" `shouldBe` Right (amount "89")

  it "parses a sum with negative values" $
    parseAmount HL.nulljournal "-42 + 23 + 42 - 23 + 1" `shouldBe` Right (amount "1")

  it "fails to parse a trailing plus" $
    parseAmount HL.nulljournal "23 +" `shouldSatisfy` isLeft

amount :: Text -> HL.MixedAmount
amount = HL.mixed . Just . fromRight . runIdentity . runParserT (evalStateT HL.amountp HL.nulljournal) ""

fromRight :: Either a b -> b
fromRight = either (error "fromRight: Left value encountered") id
