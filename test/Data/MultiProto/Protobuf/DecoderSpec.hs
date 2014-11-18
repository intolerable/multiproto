module Data.MultiProto.Protobuf.DecoderSpec
  (main, spec) where

import Data.MultiProto.Protobuf.Decoder
import Data.MultiProto.Protobuf.Encoder

import Control.Applicative
import Data.Serialize
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.ByteString as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "varint" $ do
    prop "decode . encode == id" $ do
      n <- arbitrary `suchThat` (> 0)
      return $ runGet varintInteger (runPut (putVarint n)) == Right (n :: Integer)

  describe "field" $ do
    prop "decode . encode == id" $ do
      n <- arbitrary `suchThat` (> 0)
      t <- elements [Varint, Bit64, LengthDelimited, Bit32]
      return $ runGet field (runPut (putFieldLabel n t)) == Right (n, t)

  describe "putLengthDelimited" $ do
    prop "decode . encode == id" $ do
      b <- ByteString.pack <$> arbitrary
      return $ runGet parseLengthDelimited (runPut (putLengthDelimited b)) == Right b
