module Data.MultiProto.Protobuf.DecoderSpec
  (main, spec) where

import Data.MultiProto.Protobuf.Decoder ()

import Data.Serialize ()
import Test.Hspec
import qualified Data.ByteString as ByteString ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  (return () :: Spec)
