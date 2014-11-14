module Data.MultiProto.Protobuf.Parser where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (option)
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Attoparsec.ByteString.Char8 as Parser

data Proto = Message'
           | Extend'
           | Enum ByteString [EnumInner]
           | Import ByteString
           | Package'
           | Option' Option
           | Empty
  deriving (Show, Read, Eq)

data Option = Option [ByteString] Literal
  deriving (Show, Read, Eq)

data Literal = Identifier ByteString
             | StringLit ByteString
             | IntegerLit Integer
  deriving (Show, Read, Eq)

data EnumInner = EnumOption Option
               | EnumField Integer ByteString
  deriving (Show, Read, Eq)

protos :: Parser [Proto]
protos = (proto `sepBy` skipSpace) <* endOfInput

proto :: Parser Proto
proto = import_

message :: Parser Proto
message = pure Empty

enum :: Parser Proto
enum = do
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
enumField = do
  name <- identifier
  skipSpace
  "="
  skipSpace
  num <- integerLit
  skipSpace
  ";"
  return $ EnumField num name

identifier :: Parser ByteString
identifier = do
  c <- satisfy (inClass "a-zA-z_")
  rest <- Parser.takeWhile (inClass "a-zA-Z0-9_")
  return $ ByteString.cons c rest

camelIdentifier :: Parser ByteString
camelIdentifier = do
  c <- satisfy (inClass "A-z")
  rest <- Parser.takeWhile (inClass "a-zA-Z0-9_")
  return $ ByteString.cons c rest

extend :: Parser Proto
extend = pure Empty

import_ :: Parser Proto
import_ = Import <$> do
  "import"
  skipSpace
  res <- stringLit
  skipSpace
  ";"
  skipSpace
  return res

package :: Parser Proto
package = pure Empty

option :: Parser Option
option = do
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
constant =
  (Identifier <$> identifier) <|>
  (IntegerLit <$> integerLit) <|>
  (StringLit <$> stringLit)

stringLit :: Parser ByteString
stringLit = "\"" *> takeTill (=='"') <* "\""

whitespace :: Parser ()
whitespace = " " >> pure ()

integerLit :: Parser Integer
integerLit = do
  digits <- Parser.takeWhile isDigit
  skipSpace
  readParse digits

readParse :: Read a => ByteString -> Parser a
readParse s =
  case readMaybe $ ByteString.unpack s of
    Just res -> pure res
    Nothing -> fail "no read"
