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

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
