
{-# LANGUAGE TemplateHaskell #-}

module Data.RecordModel.Parser where

import Language.Haskell.TH
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import Control.Applicative(pure, (<*>))


type Parser = ParsecT String () (State SourcePos)


languageDef :: GenLanguageDef String () (State SourcePos)
languageDef = P.LanguageDef
   {
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames = ["String"],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = [],
      P.caseSensitive  = True
   }


lexer :: P.GenTokenParser String () (State SourcePos)
lexer = P.makeTokenParser languageDef


reserved :: String -> Parser ()
reserved = P.reserved lexer


identifier :: Parser String
identifier = P.identifier lexer


data Field = Field
  {
    fieldName :: !String,
    fieldType :: Type
  }


data Model = Model
  {
    modelName :: !String,
    modelFields :: [Field]
  }


parseType :: Parser Type
parseType = choice 
  [
    reserved "String" >> return (ConT ''String)
  ]


parseField :: Parser Field
parseField = pure Field <*> identifier <*> parseType