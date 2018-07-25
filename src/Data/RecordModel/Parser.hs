
module Data.RecordModel.Parser(parse) where

import Control.Applicative((<$>), (<*>))
import Data.Functor.Identity
import Language.Haskell.TH
import Text.Parsec hiding(State, parse)
import Text.Parsec.Language
import Text.Parsec.Indent
import qualified Text.Parsec.Token as P

import Data.RecordModel.Model


data ModelAst
  = FieldAst !String !Type
  | DerivingAst [String]


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
  withBlock createModel identifier parseModelAst
  where
    createModel name modelAsts = 
      let
        isFieldAst (FieldAst _ _) = True
        isFieldAst _ = False
        isDerivingAst = not . isFieldAst 
        toField (FieldAst n t) = Field n n t
        toDeriving (DerivingAst ds) = ds
        fields = map toField . filter isFieldAst $ modelAsts
        derivings = map toDeriving . filter isDerivingAst $ modelAsts
      in
        Model name fields $ "Generic" : concat derivings


parseType :: Parser Type
parseType = do
  spaces
  name <- identifier
  return $ ConT $ mkName name


parseModelAst :: Parser ModelAst
parseModelAst = do
  spaces
  choice 
    [
      parseFieldAst,
      parseDerivingAst      
    ]


parseDerivingAst :: Parser ModelAst
parseDerivingAst = do
  reserved "deriving"
  names <- many1 identifier
  return $ DerivingAst names


parseFieldAst :: Parser ModelAst
parseFieldAst = FieldAst <$> identifier <*> parseType


parse :: SourceName -> String -> [Model]
parse sourceName input = case runIndentParser parseModels () sourceName input of
  Left err -> error $ show err
  Right ms -> ms
