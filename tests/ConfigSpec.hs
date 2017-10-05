{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Test.Hspec

import Config

spec :: Spec
spec = do
  parseConfigTextSpec
  replaceLeadingTildeSpec


parseConfigTextSpec :: Spec
parseConfigTextSpec = describe "parseConfigText" $ do
  it "parses the pretty printed config" $
    parseConfigText "" (prettyPrintConfig defaultConfig)
    `shouldBe` Right defaultConfig

  it "parses the empty config" $
    parseConfigText "" "" `shouldBe` Right defaultConfig

replaceLeadingTildeSpec :: Spec
replaceLeadingTildeSpec = describe "replaceLeadingTilde" $ do
  it "expands the tilde if followed by a slash" $
    replaceLeadingTilde "~/foo/bar" "baz" `shouldBe` "baz/foo/bar"

  it "doesn't expand the tilde if not followed by a slash" $
    replaceLeadingTilde "~foo/bar" "baz" `shouldBe` "~foo/bar"
