{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigParserSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Text (Text)
import qualified Data.Text as T

import           ConfigParser

spec :: Spec
spec = do
  fullTest
  defaultTest
  syntaxTests
  valueTests
  commentTests
  exampleTests

data TestData = TestData
  { someInt :: Int
  , someInteger :: Integer
  , someString :: String
  , someText :: Text
  } deriving (Eq, Show)

testParser :: OptParser TestData
testParser = TestData
  <$> option "someInt" 42 "Help for this"
  <*> option "someInteger" 23 "Help for that"
  <*> option "someString" "foobar" "Help with\nMultiple lines"
  <*> option "someText" "barfoo" "And another help"

defaultData :: TestData
defaultData = parserDefault testParser

fullTest :: Spec
fullTest = it "parses a complete example" $ do
  let inputTxt = T.unlines [ "someInt = 1"
                           , "someInteger = 2"
                           , "someString = a"
                           , "someText = \"b\""]
      output = TestData 1 2 "a" "b"
  parseConfig "" inputTxt testParser `shouldBe` Right output

defaultTest :: Spec
defaultTest = do
  it "fills in the default values" $
    parseConfig "" "" testParser `shouldBe` Right (TestData 42 23 "foobar" "barfoo")

  it "fills in the default values for random data" $
    property defaultWorksProp

defaultWorksProp :: TestData -> Property
defaultWorksProp testData =
  let parser = TestData
                 <$> option "someInt" (someInt testData) "Help for this"
                 <*> option "someInteger" (someInteger testData) "Help for that"
                 <*> option "someString" (someString testData) "Help with\nMultiple lines"
                 <*> option "someText" (someText testData) "And another help"
  in parseConfig "" "" parser === Right testData


syntaxTests :: Spec
syntaxTests = do
  context "given whitespace" whitespaceTests
  context "given escaped strings" escapingTests
  context "given bare strings" bareStringTests
  optionNameTests

whitespaceTests :: Spec
whitespaceTests = do
  it "parses just whitespace" $
    parseConfig "" "" testParser
      `shouldBe` Right (TestData 42 23 "foobar" "barfoo")

  it "parses beginning whitespace" $
    parseConfig "" "\n\n\n   someInt = 13" testParser
      `shouldBe` Right (TestData 13 23 "foobar" "barfoo")

  it "parses trailing whitespace" $
    parseConfig "" "someInt = 13    \n\n\n" testParser
      `shouldBe` Right (TestData 13 23 "foobar" "barfoo")

  it "parses middle whitespace" $
    parseConfig "" "someInt = 13    \n\n\n    someInteger = 14" testParser
      `shouldBe` Right (TestData 13 14 "foobar" "barfoo")

  it "parses whitespace everywhere" $
    parseConfig "" " \n \n  someInt = 13    \n \n \n    someInteger = 14   \n \n  " testParser
      `shouldBe` Right (TestData 13 14 "foobar" "barfoo")

escapingTests :: Spec
escapingTests = do
  it "parses simple escaped strings" $
    parseConfig "" "someText = \"test\"  " testParser
      `shouldBe` Right (defaultData { someText = "test" })

  it "parses escaped strings with quotes in them" $
    parseConfig "" "someText = \"te\\\"st\"  " testParser
      `shouldBe` Right (defaultData { someText = "te\"st" })

  it "parses escaped strings with backslashes in them" $
    parseConfig "" "someText = \"te\\\\st\"  " testParser
      `shouldBe` Right (defaultData { someText = "te\\st" })

  it "parses escaped strings with newlines in them" $
    parseConfig "" "someText = \"te\nst\"  " testParser
      `shouldBe` Right (defaultData { someText = "te\nst" })

  it "fails to parse non-terminated escaped strings" $
    parseConfig "" "someText = \"test  " testParser
      `shouldSatisfy` isLeft


bareStringTests :: Spec
bareStringTests = do
  it "parses a bare string correctly" $
    parseConfig "" "someText =test" testParser
      `shouldBe` Right (defaultData { someText = "test" })

  it "correctly trims bare strings" $
    parseConfig "" "someText =   foo test   " testParser
      `shouldBe` Right (defaultData { someText = "foo test" })

  it "fails to parse empty bare strings" $
    parseConfig "" "someText = " testParser `shouldSatisfy` isLeft

optionNameTests :: Spec
optionNameTests = do
  it "allows dashes in option names" $ do
    let parser = (\x -> defaultData { someInt = x }) <$> option "test-name" 10 ""
    parseConfig "" "test-name = 10" parser `shouldBe` Right defaultData { someInt = 10 }

  it "allows underscores in option names" $ do
    let parser = (\x -> defaultData { someInt = x }) <$> option "test_name" 10 ""
    parseConfig "" "test_name = 10" parser `shouldBe` Right defaultData { someInt = 10 }

  it "doesn't allow spaces in option names" $ do
    let parser = (\x -> defaultData { someInt = x }) <$> option "test name" 10 ""
    parseConfig "" "test name = 10" parser `shouldSatisfy` isLeft

  it "doesn't allow equal signs in option names" $ do
    let parser = (\x -> defaultData { someInt = x }) <$> option "test=foo" 10 ""
    parseConfig "" "test=foo = 10" parser `shouldSatisfy` isLeft

valueTests :: Spec
valueTests = do
  context "given integers" $ do
    it "parses zero" $
      parseConfig "" "someInt = 0" testParser `shouldBe` Right defaultData { someInt = 0 }

    it "parses negative zero" $
      parseConfig "" "someInt = -0" testParser `shouldBe` Right defaultData { someInt = 0 }

    it "fails to parse integer with trailing stuff" $
      parseConfig "" "someInt = 10foo" testParser `shouldSatisfy` isLeft

    it "fails to parse empty string as integer" $
      parseConfig "" "someInt = \"\"" testParser `shouldSatisfy` isLeft

    it "fails to parse letters as integer" $
      parseConfig "" "someInt = foo" testParser `shouldSatisfy` isLeft


  context "given strings" $
    it "parses the empty string quoted" $
      parseConfig "" "someString = \"\"" testParser `shouldBe` Right defaultData { someString = "" }

commentTests :: Spec
commentTests = do
  it "handles a file with just comments" $
    parseConfig "" "# a comment \n  #another comment  " testParser
      `shouldBe` Right defaultData

  it "handles comments and whitespace in front" $
    parseConfig "" "  \n\n#another comment  " testParser
      `shouldBe` Right defaultData

  it "handles comments and whitespace in front" $
    parseConfig "" "  \n\n#another comment  " testParser
      `shouldBe` Right defaultData

  it "handles comments and whitespace after" $
    parseConfig "" "#another comment\n\n  " testParser
      `shouldBe` Right defaultData

  it "handles comments with whitespace between" $
    parseConfig "" "\n \n # comment \n #another comment\n\n  " testParser
      `shouldBe` Right defaultData

  it "handles comments after assignments" $ do
    parseConfig "" "someInt = 4# a comment" testParser
      `shouldBe` Right defaultData { someInt = 4 }

    parseConfig "" "someInt = 4# a comment\n" testParser
      `shouldBe` Right defaultData { someInt = 4 }

    parseConfig "" "someInt = 4  # a comment" testParser
      `shouldBe` Right defaultData { someInt = 4 }

  it "handles comments around assignments" $ do
    parseConfig "" "someInt = 4# a comment\n # a comment\nsomeString = foo # bar" testParser
      `shouldBe` Right defaultData { someInt = 4, someString = "foo" }


exampleTests :: Spec
exampleTests = describe "parserExample" $ do
  it "works for one example" $
    let output = T.strip $ T.unlines
          [ "# Help for this"
          , "someInt = 42"
          , ""
          , "# Help for that"
          , "someInteger = 23"
          , ""
          , "# Help with"
          , "# Multiple lines"
          , "someString = \"foobar\""
          , ""
          , "# And another help"
          , "someText = \"barfoo\""
          ]
    in parserExample testParser `shouldBe` output

  it "can parse it's own example output" $
    property exampleParseableProp

exampleParseableProp :: TestData -> Property
exampleParseableProp testData =
  let parser = TestData <$> option "someInt" (someInt testData) "help"
                        <*> option "someInteger" (someInteger testData) "help"
                        <*> option "someString" (someString testData) "help"
                        <*> option "someText" (someText testData) "help"
  in parseConfig "" (parserExample parser) parser === Right testData

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

instance Arbitrary TestData where
  arbitrary = TestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
