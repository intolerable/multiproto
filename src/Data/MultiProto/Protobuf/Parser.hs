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
           | ProtoExtend
           | ProtoEnum Enum
           | ProtoImport ByteString
           | ProtoPackage ByteString
           | ProtoOption Option
           | Empty
  deriving (Show, Read, Eq)

data Message = Message ByteString [MessageInner]
  deriving (Show, Read, Eq)

data MessageInner = MessageField Field
                  | MessageEnum Enum
                  | NestedMessage Message
                  | MessageExtend
                  | MessageExtensions
                  | MessageOption Option
  deriving (Show, Read, Eq)

data Field = Field
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
  extend <|>
  (ProtoEnum <$> enum) <|>
  import_ <|>
  package <|>
  (ProtoOption <$> option) <|>
  (";" *> pure Empty)

comment :: Parser Proto
comment = label "comment" $ do
  "//"
  takeTill (inClass "\r\n")
  skipSpace
  pure Empty

message :: Parser Message
message = label "message" $ do
  "message"
  skipSpace
  iden <- identifier
  skipSpace
  "{"
  skipSpace
  res <- messageBody
  skipSpace
  "}"
  return $ Message iden res

messageBody :: Parser [MessageInner]
messageBody = label "messageBody" $ undefined

enum :: Parser Enum
enum = label "enum" $ do
  "enum"
  skipSpace
  res <- identifier
  skipSpace
  "{"
  skipSpace
  fields <- many enumField
  skipSpace
  "}"
  skipSpace
  return $ Enum res fields

enumField :: Parser EnumInner
enumField = label "enumField" $ do
  name <- identifier
  skipSpace
  "="
  skipSpace
  num <- integerLit
  skipSpace
  ";"
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

extend :: Parser Proto
extend = undefined

import_ :: Parser Proto
import_ = label "import_" $ do
  "import"
  skipSpace
  res <- stringLit
  skipSpace
  ";"
  skipSpace
  return $ ProtoImport res

package :: Parser Proto
package = label "package" $ do
  "package"
  skipSpace
  res <- stringLit
  skipSpace
  ";"
  skipSpace
  return $ ProtoPackage res

option :: Parser Option
option = label "option" $ do
  "option"
  skipSpace
  ids <- identifier `sepBy` "."
  skipSpace
  "="
  skipSpace
  res <- constant
  skipSpace
  ";"
  skipSpace
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
