module Data.MultiProto.Protobuf.ParserSpec
  (main, spec) where

import Data.MultiProto.Protobuf.Parser

import Data.Attoparsec.ByteString.Char8
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stringLit" $ do
    let p = parseOnly stringLit

    it "should be able to parse basic string literals" $ do
      p "\"hello\"" `shouldBe` Right "hello"
      p "'hello'" `shouldBe` Right "hello"

    it "should be able to parse string literals with escaped characters" $ do
      p "\"hel\\\"lo\"" `shouldBe` Right "hel\"lo"
      p "\"\t\"" `shouldBe` Right "\t"
      p "\"\'\"" `shouldBe` Right "'"

    it "shouldn't parse invalid string literals" $ do
      p "\"" `shouldSatisfy` isLeft
      p "\"\'" `shouldSatisfy` isLeft

  describe "identifier" $ do
    let p = parseOnly identifier

    it "should be able to parse identifiers" $ do
      p "hello" `shouldBe` Right "hello"
      p "h_ello" `shouldBe` Right "h_ello"
      p "_hello" `shouldBe` Right "_hello"

  describe "capitalIdentifier" $ do
    let p = parseOnly capitalIdentifier

    it "should be able to parse identifiers w/ leading capitals" $ do
      p "Hello" `shouldBe` Right "Hello"
      p "HELLO" `shouldBe` Right "HELLO"

    it "should fail for identifiers w/o leading caps" $ do
      p "hello" `shouldSatisfy` isLeft

  describe "integerLit" $ do
    let p = parseOnly integerLit

    it "should be able to parse integer literals" $ do
      p "5" `shouldBe` Right 5

    it "should fail for things that aren't integers" $ do
      p "5.5" `shouldSatisfy` isLeft
      p "no" `shouldSatisfy` isLeft

  describe "hexLit" $ do
    let p = parseOnly hexLit

    it "should be able to parse hex literals" $ do
      p "0xff" `shouldBe` Right 0xff
      p "0X0" `shouldBe` Right 0x0
      p "0xDEADBEEF" `shouldBe` Right 0xDEADBEEF

    it "should fail on invalid hex literals" $ do
      p "0xno" `shouldSatisfy` isLeft
      p "0x" `shouldSatisfy` isLeft

  describe "boolLit" $ do
    let p = parseOnly boolLit

    it "should be able to parse boolean literals" $ do
      p "true" `shouldBe` Right True
      p "false" `shouldBe` Right False

    it "should fail for things that aren't boolean literals" $ do
      p "True" `shouldSatisfy` isLeft
      p "FALSE" `shouldSatisfy` isLeft
      p "0" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
