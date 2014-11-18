module Data.MultiProto.Protobuf.Decoder where

import Data.Bits
import Data.Serialize (Get)
import Language.Literals.Binary
import qualified Data.Serialize.Get as S

data Type = Varint
          | Bit64
          | LengthDelimited
          | StartGroup
          | EndGroup
          | Bit32
  deriving (Show, Read, Eq, Enum)

varint :: (Integral a, Bits a) => Get a
varint = do
  w <- S.getWord8
  if w `testBit` 7
    then do
      n <- varint
      return $ (n `shiftL` 7) + fromIntegral (w `clearBit` 7)
    else return $ fromIntegral $ w `clearBit` 7

field :: Get (Type, Int)
field = do
  v <- varint
  return (toEnum $ v .&. [b| 0000 0111 |], v `shiftR` 3)

