module Data.MultiProto.Protobuf.Decoder where

import Control.Applicative
import Data.Bits
import Data.ByteString (ByteString)
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

data Value = VarintValue Integer
           | ByteStringValue ByteString
  deriving (Show, Read, Eq)

varint :: (Integral a, Bits a) => Get a
varint = do
  w <- S.getWord8
  if w `testBit` 7
    then do
      n <- varint
      return $ (n `shiftL` 7) + fromIntegral (w `clearBit` 7)
    else return $ fromIntegral $ w `clearBit` 7

field :: Get (Int, Type)
field = do
  v <- varint
  return (v `shiftR` 3, toEnum $ v .&. [b| 0000 0111 |])

parseBit64 :: Get ByteString
parseBit64 = S.getBytes 8

parseBit32 :: Get ByteString
parseBit32 = S.getBytes 4

parseLengthDelimited :: Get ByteString
parseLengthDelimited = varint >>= S.getBytes

parseFullField :: Get (Type, Int, Value)
parseFullField = do
  (i, t) <- field
  (,,) <$> pure t <*> pure i <*> case t of
    Varint -> VarintValue <$> varint
    Bit64 -> ByteStringValue <$> parseBit64
    LengthDelimited -> ByteStringValue <$> parseLengthDelimited
    Bit32 -> ByteStringValue <$> parseBit32
    _ -> fail "what"

getUntilConsumed :: Get a -> Get [a]
getUntilConsumed p = go []
  where
    go xs = do
      e <- S.isEmpty
      if e
        then return xs
        else do
          (:) <$> p <*> getUntilConsumed p

