module Data.MultiProto.Protobuf.ParserSpec
  (main, spec) where

import Data.MultiProto.Protobuf.Parser

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Either (isLeft)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stringLit" $ do
    let p = parseEnd stringLit

    it "should be able to parse basic string literals" $ do
      p "\"hello\"" `shouldBe` Right "hello"
      p "'hello'" `shouldBe` Right "hello"

    it "should be able to parse string literals with escaped characters" $ do
      p "\"hel\\\"lo\"" `shouldBe` Right "hel\\\"lo"
      p "\"\t\"" `shouldBe` Right "\t"
      p "\"\'\"" `shouldBe` Right "'"

    it "shouldn't parse invalid string literals" $ do
      p "what" `shouldSatisfy` isLeft
      p "\"" `shouldSatisfy` isLeft
      p "\"\'" `shouldSatisfy` isLeft

  describe "escapeHex" $ do
    let p = parseEnd escapeHex

    it "should be able to parse escape hexes in strings" $ do
      p "\\xAF" `shouldBe` Right "\\xAF"
      p "\\X0F" `shouldBe` Right "\\X0F"

    it "shouldn't parse other hexes" $ do
      p "0xAF" `shouldSatisfy` isLeft

  describe "escapeOct" $ do
    let p = parseEnd escapeOct

    it "should be able to parse escape octals in strings" $ do
      p "\\007" `shouldBe` Right "\\007"
      p "\\03" `shouldBe` Right "\\03"
      p "\\0005" `shouldBe` Right "\\0005"

    it "shouldn't parse non-octs" $ do
      p "0006" `shouldSatisfy` isLeft

  describe "identifier" $ do
    let p = parseEnd identifier

    it "should be able to parse identifiers" $ do
      p "hello" `shouldBe` Right "hello"
      p "h_ello" `shouldBe` Right "h_ello"
      p "_hello" `shouldBe` Right "_hello"

  describe "capitalIdentifier" $ do
    let p = parseEnd capitalIdentifier

    it "should be able to parse identifiers w/ leading capitals" $ do
      p "Hello" `shouldBe` Right "Hello"
      p "HELLO" `shouldBe` Right "HELLO"

    it "should fail for identifiers w/o leading caps" $ do
      p "hello" `shouldSatisfy` isLeft

  describe "integerLit" $ do
    let p = parseEnd integerLit

    it "should be able to parse integer literals" $ do
      p "5" `shouldBe` Right 5

    it "should fail for things that aren't integers" $ do
      p "5.5" `shouldSatisfy` isLeft
      p "no" `shouldSatisfy` isLeft

  describe "hexLit" $ do
    let p = parseEnd hexLit

    it "should be able to parse hex literals" $ do
      p "0xff" `shouldBe` Right 0xff
      p "0X0" `shouldBe` Right 0x0
      p "0xDEADBEEF" `shouldBe` Right 0xDEADBEEF

    it "should fail on invalid hex literals" $ do
      p "0xno" `shouldSatisfy` isLeft
      p "0x" `shouldSatisfy` isLeft

  describe "floatLit" $ do
    let p = parseEnd floatLit

    it "should be able to parse float literals" $ do
      p "5.5" `shouldBe` Right 5.5
      p "64" `shouldBe` Right 64.0
      p "100.0e-2" `shouldBe` Right 1.0

    it "shouldn't parse things that aren't floats" $ do
      p "carrot" `shouldSatisfy` isLeft
      p "192.168.0.1" `shouldSatisfy` isLeft

  describe "boolLit" $ do
    let p = parseEnd boolLit

    it "should be able to parse boolean literals" $ do
      p "true" `shouldBe` Right True
      p "false" `shouldBe` Right False

    it "should fail for things that aren't boolean literals" $ do
      p "True" `shouldSatisfy` isLeft
      p "FALSE" `shouldSatisfy` isLeft
      p "0" `shouldSatisfy` isLeft

parseEnd :: Parser a -> ByteString -> Either String a
parseEnd p = parseOnly (p <* endOfInput)
