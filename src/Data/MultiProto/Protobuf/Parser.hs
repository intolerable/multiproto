{-# OPTIONS -fno-warn-unused-do-bind #-}

module Data.MultiProto.Protobuf.Parser where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (option)
import Text.Read (readMaybe)
import Prelude hiding (Enum)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Attoparsec.ByteString.Char8 as Parser

data Proto = ProtoMessage Message
           | ProtoExtend Extend
           | ProtoEnum Enum
           | ProtoImport ByteString
           | ProtoPackage ByteString
           | ProtoOption Option
           | Empty
  deriving (Show, Read, Eq)

data Message = Message ByteString [MessageInner]
  deriving (Show, Read, Eq)

data Extend = Extend ByteString [MessageInner]
  deriving (Show, Read, Eq)

data MessageInner = MessageField Field
                  | MessageEnum Enum
                  | NestedMessage Message
                  | MessageExtend Extend
                  | MessageExtensions
                  | MessageOption Option
  deriving (Show, Read, Eq)

data Field = Field Modifier Type ByteString Integer [Option]
  deriving (Show, Read, Eq)

type Type = ByteString

data Modifier = Required
              | Optional
              | Repeated
  deriving (Show, Read, Eq)

data Option = Option [ByteString] Literal
  deriving (Show, Read, Eq)

data Literal = Identifier ByteString
             | StringLit ByteString
             | IntegerLit Integer
             | BoolLit Bool
  deriving (Show, Read, Eq)

data Enum = Enum ByteString [EnumInner]
  deriving (Show, Read, Eq)

data EnumInner = EnumOption Option
               | EnumField Integer ByteString
  deriving (Show, Read, Eq)

protos :: Parser [Proto]
protos = label "protos" $
  many proto <* endOfInput

proto :: Parser Proto
proto = label "proto" $
  comment <|>
  (ProtoMessage <$> message) <|>
  (ProtoExtend <$> extend) <|>
  (ProtoEnum <$> enum) <|>
  import_ <|>
  package <|>
  (ProtoOption <$> option) <|>
  (";" *> pure Empty)

comment :: Parser Proto
comment = label "comment" $ do
  "//"
  ss $ takeTill (inClass "\r\n")
  pure Empty

message :: Parser Message
message = label "message" $ do
  ss "message"
  iden <- ss identifier
  ss "{"
  res <- ss messageBody
  ss "}"
  return $ Message iden res

messageBody :: Parser [MessageInner]
messageBody = label "messageBody" $ do
  ss "{"
  inners <- ss $ many $
    (MessageField <$> messageField) <|>
    (MessageEnum <$> enum) <|>
    (NestedMessage <$> message) <|>
    (MessageExtend <$> extend)
  ss "}"
  return inners

messageField :: Parser Field
messageField = do
  m <- ss modifier
  t <- ss type_
  i <- ss identifier
  ss "="
  n <- ss integerLit
  ss ";"
  return $ Field m t i n []

modifier :: Parser Modifier
modifier =
  ("required" *> pure Required) <|>
  ("optional" *> pure Optional) <|>
  ("repeated" *> pure Repeated)

type_ :: Parser Type
type_ = undefined

enum :: Parser Enum
enum = label "enum" $ do
  ss "enum"
  res <- ss identifier
  ss "{"
  fields <- ss $ many enumField
  ss "}"
  return $ Enum res fields

enumField :: Parser EnumInner
enumField = label "enumField" $ do
  name <- ss identifier
  ss "="
  num <- ss integerLit
  ss ";"
  return $ EnumField num name

identifier :: Parser ByteString
identifier = label "identifier" $ do
  c <- satisfy (inClass "a-zA-z_")
  rest <- Parser.takeWhile (inClass "a-zA-Z0-9_")
  return $ ByteString.cons c rest

capitalIdentifier :: Parser ByteString
capitalIdentifier = label "capitalIdentifier" $ do
  c <- satisfy (inClass "A-Z")
  rest <- Parser.takeWhile (inClass "a-zA-Z0-9_")
  return $ ByteString.cons c rest

extend :: Parser Extend
extend = undefined

import_ :: Parser Proto
import_ = label "import_" $ do
  ss "import"
  res <- ss stringLit
  ss ";"
  return $ ProtoImport res

package :: Parser Proto
package = label "package" $ do
  ss "package"
  res <- ss stringLit
  ss ";"
  return $ ProtoPackage res

option :: Parser Option
option = label "option" $ do
  ss "option"
  ids <- ss $ identifier `sepBy` "."
  ss "="
  res <- ss constant
  ss ";"
  return $ Option ids res

constant :: Parser Literal
constant = label "constant" $
  (Identifier <$> identifier) <|>
  (IntegerLit <$> integerLit) <|>
  (StringLit <$> stringLit) <|>
  (BoolLit <$> boolLit)

stringLit :: Parser ByteString
stringLit = label "stringLit" $
  ("\"" *> takeTill (=='"') <* "\"") <|>
  ("'" *> takeTill (=='\'') <* "'")

integerLit :: Parser Integer
integerLit = label "integerLit" $
  readParse =<< Parser.takeWhile (inClass "0-9.")

hexLit :: Parser Integer
hexLit = label "hexLit" $ do
  front <- "0x" <|> "0X"
  digits <- Parser.takeWhile (inClass "A-Fa-f0-9")
  readParse $ ByteString.append front digits

boolLit :: Parser Bool
boolLit = ("true" *> pure True) <|>
          ("false" *> pure False)

readParse :: Read a => ByteString -> Parser a
readParse s =
  case readMaybe $ ByteString.unpack s of
    Just res -> pure res
    Nothing -> fail "no read"

label :: String -> Parser a -> Parser a
label n p = p <?> n

ss :: Parser a -> Parser a
ss p = p <* skipSpace
