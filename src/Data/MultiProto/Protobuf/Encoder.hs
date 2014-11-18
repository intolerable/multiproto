module Data.MultiProto.Protobuf.Encoder where

import Data.MultiProto.Protobuf.Decoder (Value(..), Type(..))

import Data.Bits
import Data.Serialize
import Prelude hiding (splitAt)

putVarint :: (Bits a, Integral a) => a -> Put
putVarint v = do
  let (shifted, sect) = v `splitAt` 7
  if shifted == 0
    then putWord8 $ fromIntegral sect
    else do
      putWord8 $ fromIntegral $ sect `setBit` 7
      putVarint shifted

putFieldLabel :: Int -> Type -> Put
putFieldLabel n t =
  putVarint $ n `shiftL` 3 + fromEnum t

putField :: Int -> Type -> Value -> Put
putField n t v = do
  putFieldLabel n t
  putValue v

putValue :: Value -> Put
putValue (VarintValue v) = putVarint v
putValue (ByteStringValue v) = putByteString v

splitAt :: Bits a => a -> Int -> (a, a)
splitAt v n = (v `shiftR` n, v .&. (foldl setBit zeroBits [0..n-1]))
