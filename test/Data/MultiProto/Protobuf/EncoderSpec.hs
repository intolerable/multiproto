module Data.MultiProto.Protobuf.EncoderSpec
  (main, spec) where

import Data.MultiProto.Protobuf.Decoder
import Data.MultiProto.Protobuf.Encoder

import Data.Serialize
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Applicative
import qualified Data.ByteString as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "putValue" $ do
    it "should be able to put bytestrings" $ do
      let p = ByteString.unpack . runPut . putValue . ByteStringValue . ByteString.pack
      p [0x50] `shouldBe` [0x50]
      p [0x50, 0x00] `shouldBe` [0x50, 0x00]
      p [0xFF, 0xFF] `shouldBe` [0xFF, 0xFF]

    prop "putting a bytestring is the same as the bytestring" $ do
      b <- ByteString.pack <$> arbitrary
      return $ runPut (putValue (ByteStringValue b)) == b

  describe "putFieldLabel" $ do
    it "should be able to put field labels" $ do
      let p n t = ByteString.unpack $ runPut $ putFieldLabel n t
      p 1 Varint `shouldBe` [0x08]
      p 2 Varint `shouldBe` [0x10]

  describe "putVarint" $ do
    it "should be able to put a variety of numeric types correctly" $ do
      let p = ByteString.unpack . runPut . putVarint
      p (5 :: Integer) `shouldBe` [0x05]
      p (150 :: Integer) `shouldBe` [0x96, 0x01]

  describe "putField" $ do
    it "should be able to spit out a full field" $ do
      let p n t v = ByteString.unpack $ runPut $ putField n t v
      p 1 Varint (VarintValue 150) `shouldBe` [0x08, 0x96, 0x01]

    prop "should have the same output as a putFieldLabel -> putValue" $ do
      b <- arbitrary `suchThat` (> 0)
      return $ runPut (putField 1 Varint (VarintValue b)) ==
        runPut (putFieldLabel 1 Varint >> putValue (VarintValue b))
