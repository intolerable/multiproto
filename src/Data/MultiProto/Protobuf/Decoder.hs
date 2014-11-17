module Data.MultiProto.Protobuf.Decoder where

import Data.Serialize

getSingleByteInt :: Get Integer
getSingleByteInt = get
