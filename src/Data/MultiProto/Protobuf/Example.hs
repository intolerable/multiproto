module Data.MultiProto.Protobuf.Example where

import Control.Applicative
import Data.Default
import Data.MultiProto.Protobuf.Decoder
import Data.Semigroup
import Data.Serialize

data Example =
  Example { requiredField :: Int
          , optionalField :: Maybe Int }
  deriving (Show, Read, Eq)

instance Serialize Example where
  get = undefined
  put = undefined

instance Semigroup Example where
  _ <> (Example r2 o2) = Example r2 o2

makeConcrete :: ExampleW -> Maybe Example
makeConcrete (ExampleW r o) = do
  r' <- getLast <$> getOption r
  let o' = getLast <$> getOption o
  return $ Example r' o'

data ExampleW =
  ExampleW { requiredFieldW :: Option (Last Int)
           , optionalFieldW :: Option (Last Int) }
  deriving (Show, Read, Eq)

instance Semigroup ExampleW where
  (ExampleW r1 o1) <> (ExampleW r2 o2) = ExampleW (r1 <> r2) (o1 <> o2)

instance Monoid ExampleW where
  mempty = ExampleW (Option Nothing) (Option Nothing)
  mappend = (<>)

instance Default ExampleW where
  def = mempty

instance Serialize ExampleW where
  get = undefined
  put = undefined

