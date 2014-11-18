{-# OPTIONS -fno-warn-unused-do-bind #-}

module Data.MultiProto.Protobuf.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (option)
import Data.ByteString.Char8 (ByteString)
import Prelude hiding (Enum)
import Text.Read (readMaybe)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as ByteString

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

data Extend = Extend UserType [MessageInner]
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

data Type = TDouble
          | TFloat
          | TInt32
          | TInt64
          | TUInt32
          | TUInt64
          | TSInt32
          | TSInt64
          | TFixed32
          | TFixed64
          | TSFixed32
          | TSFixed64
          | TBool
          | TString
          | TBytes
          | TUser UserType
  deriving (Show, Read, Eq)

data UserType = FullyQualified [ByteString]
              | InScope [ByteString]
  deriving (Show, Read, Eq)

data Modifier = Required
              | Optional
              | Repeated
  deriving (Show, Read, Eq)

data Option = Option [ByteString] Literal
  deriving (Show, Read, Eq)

data Literal = Identifier ByteString
             | StringLit ByteString
             | IntegerLit Integer
             | FloatLit Double
             | BoolLit Bool
  deriving (Show, Read, Eq)

data Enum = Enum ByteString [EnumInner]
  deriving (Show, Read, Eq)

data EnumInner = EnumOption Option
               | EnumField ByteString Integer
  deriving (Show, Read, Eq)

protos :: Parser [Proto]
protos = many proto <* endOfInput

proto :: Parser Proto
proto = choice
  [ lineComment *> pure Empty
  , blockComment *> pure Empty
  , ProtoMessage <$> message
  , ProtoExtend <$> extend
  , ProtoEnum <$> enum
  , ProtoImport <$> import_
  , ProtoPackage <$> package
  , ProtoOption <$> option
  , ";" *> pure Empty ]

lineComment :: Parser ByteString
lineComment = "//" *> ss (takeTill (inClass "\r\n"))

blockComment :: Parser ByteString
blockComment = ByteString.pack <$> ss ("/*" *> manyTill anyChar "*/")

message :: Parser Message
message = ss "message" *> (Message <$> ss identifier <*> ss messageBody)

messageBody :: Parser [MessageInner]
messageBody = wrapped (ss "{") (ss "}") $ ss $ many $ choice
  [ MessageField <$> messageField
  , MessageEnum <$> enum
  , NestedMessage <$> message
  , MessageExtend <$> extend ]

messageField :: Parser Field
messageField = Field <$> ss modifier
                     <*> ss type_
                     <*> ss identifier <* ss "="
                     <*> ss integerLit
                     <*> ss options <* ss ";"

options :: Parser [Option]
options = Parser.option [] $ wrapped (ss "[") (ss "]") $ ss optionBody `sepBy1` ","

optionBody :: Parser Option
optionBody = Option <$> ss (identifier `sepBy1` ".") <*> (ss "=" *> constant)

modifier :: Parser Modifier
modifier = choice ["required" *> pure Required, "optional" *> pure Optional, "repeated" *> pure Repeated]

type_ :: Parser Type
type_ = choice
  [ "double" *> pure TDouble
  , "float" *> pure TFloat
  , "int32" *> pure TInt32
  , "int64" *> pure TInt64
  , "uint32" *> pure TUInt32
  , "uint64" *> pure TUInt64
  , "sint32" *> pure TSInt32
  , "sint64" *> pure TSInt64
  , "fixed32" *> pure TFixed32
  , "sfixed32" *> pure TSFixed32
  , "sfixed64" *> pure TSFixed64
  , "bool" *> pure TBool
  , "string" *> pure TString
  , "bytes" *> pure TBytes
  , TUser <$> userType ]

userType :: Parser UserType
userType = (FullyQualified <$> ("." *> ids)) <|> (InScope <$> ids)
  where ids = identifier `sepBy1` "."

enum :: Parser Enum
enum = Enum <$> (ss "enum" *> ss identifier)
            <*> wrapped (ss "{") (ss "}") (ss $ many enumField)

enumField :: Parser EnumInner
enumField = EnumField <$> (ss identifier <* ss "=") <*> (ss integerLit <* ss ";")

identifier :: Parser ByteString
identifier = ByteString.cons <$> satisfy (inClass "a-zA-Z_") <*> Parser.takeWhile (inClass "a-zA-Z0-9_")

capitalIdentifier :: Parser ByteString
capitalIdentifier = ByteString.cons <$> satisfy (inClass "A-Z") <*> Parser.takeWhile (inClass "a-zA-Z0-9_")

extend :: Parser Extend
extend = Extend <$> (ss "extend" *> ss userType) <*> ss messageBody

import_ :: Parser ByteString
import_ = ss "import" *> ss stringLit <* ss ";"

package :: Parser ByteString
package = ss "package" *> ss stringLit <* ss ";"

option :: Parser Option
option = Option <$> (ss "option" *> ss (identifier `sepBy` ".")) <*> (ss "=" *> ss constant <* ss ";")

constant :: Parser Literal
constant = choice
  [ Identifier <$> identifier
  , IntegerLit <$> integerLit
  , FloatLit <$> floatLit
  , StringLit <$> stringLit
  , BoolLit <$> boolLit ]

stringLit :: Parser ByteString
stringLit =
  ("\"" *> takeTill (=='"') <* "\"") <|>
  ("'" *> takeTill (=='\'') <* "'")

integerLit :: Parser Integer
integerLit = readParse =<< Parser.takeWhile (inClass "0-9.+-")

floatLit :: Parser Double
floatLit = (=<<) (readParse . fst) $ match $ do
  many1 digit
  Parser.option "" $ do
    "."
    many1 digit
  Parser.option "" $ do
    "e" <|> "E"
    Parser.option "" $ "+" <|> "-"
    many1 digit

hexLit :: Parser Integer
hexLit = (=<<) (readParse . fst) $ match $ do
  "0x" <|> "0X"
  Parser.takeWhile (inClass "A-Fa-f0-9")

boolLit :: Parser Bool
boolLit = ("true" *> pure True) <|> ("false" *> pure False)

readParse :: Read a => ByteString -> Parser a
readParse s =
  case readMaybe $ ByteString.unpack s of
    Just res -> pure res
    Nothing -> fail "no read"

ss :: Parser a -> Parser a
ss p = p <* skipSpace

wrapped :: Parser a -> Parser b -> Parser c -> Parser c
wrapped begin end middle = begin *> middle <* end
