
module Data.RecordModel.Parser(parse) where

import Control.Applicative((<$>), (<*>))
import Data.Functor.Identity
import Language.Haskell.TH
import Text.Parsec hiding(State, parse)
import Text.Parsec.Language
import Text.Parsec.Indent
import qualified Text.Parsec.Token as P

import Data.RecordModel.Model


type Parser a = IndentParser String () a


languageDef :: GenLanguageDef String () (IndentT Identity)
languageDef = emptyDef
   {
      P.commentStart = "{-",
      P.commentEnd = "-}",
      P.commentLine  = "--",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames = ["deriving"],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = [],
      P.caseSensitive  = True
   }

   
lexer :: P.GenTokenParser String () (IndentT Identity)
lexer = P.makeTokenParser languageDef


reserved :: String -> Parser ()
reserved = P.reserved lexer


identifier :: Parser String
identifier = P.identifier lexer



parseModels :: Parser [Model]
parseModels = many1 parseModel


parseModel :: Parser Model
parseModel = do
  spaces
  name <- identifier
  fields <- block parseField
  return $ Model name fields


parseType :: Parser Type
parseType = do
  spaces
  name <- identifier
  return $ ConT $ mkName name


parseField :: Parser Field
parseField = do
  spaces
  Field <$> identifier <*> parseType


parse :: SourceName -> String -> [Model]
parse sourceName input = case runIndentParser parseModels () sourceName input of
  Left err -> error $ show err
  Right ms -> ms
