
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.RecordModel.QQ(model) where

import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Data.RecordModel.Model
import Data.RecordModel.Parser

model :: QuasiQuoter
model = QuasiQuoter
   {
      quoteExp = error "Cannot use model as a expression",
      quotePat = error "Cannot use model as a pattern",
      quoteType = error "Cannot use model as a type",
      quoteDec = createDec
   }


createDec :: String -> Q [Dec]
createDec text = do
  let models = parse "" text
  let dataDs = map modelToDataD models
  toJsonInstances <- mapM modelToJSONInstance models
  fromJsonInstances <- mapM modelFromJSONInstance models
  return $ dataDs ++ concat toJsonInstances ++ concat fromJsonInstances


modelToDataD :: Model -> Dec
modelToDataD m = 
 let
   name = mkName $ modelName m
 in
   DataD [] name [] Nothing [RecC name (map fieldToVarStrictType (modelFields m))] [strToDerivClause (modelDerivings m)]


strToDerivClause :: [String] -> DerivClause
strToDerivClause names = DerivClause Nothing $ map (ConT . mkName) names


fieldToVarStrictType :: Field -> VarStrictType
fieldToVarStrictType field = (mkName (fieldName field), Bang NoSourceUnpackedness NoSourceStrictness, fieldType field)



{-

encodeModel :: Model -> Dec
encodeModel model =
  let
    mn = mkName $ modelName model
    modelValueName = mkName "m"
    toJsonName = mkName "toJSON"
    objectName = mkName "object"
    encodeFields = map (encodeField modelValueName) $ modelFields model
  in
    FunD toJsonName [Clause [VarP modelValueName] (NormalB (AppE (VarE objectName) (ListE encodeFields))) []]
-}

encodeField :: Field -> Q Exp
encodeField field = [| fromString $(stringE (fieldJsonName field)) .= first m |]


modelToJSONInstance :: Model -> Q [Dec]
modelToJSONInstance model' = do
  let encodeFields = map encodeField $ modelFields model'
  [d| instance ToJSON $(conT (mkName (modelName model'))) where toJSON m = object $(listE encodeFields) |]



decodeField :: Field -> Q Exp
decodeField field = [| fromString $(stringE (fieldJsonName field)) |]

modelFromJSONInstance :: Model -> Q [Dec]
modelFromJSONInstance model' = [d| instance FromJSON $(conT (mkName (modelName model'))) where parseJSON m = undefined |]


{-

instance ToJSON TestModel where
  toJSON m = object [fromString "first" .= first m, fromString "second" .= second m] 

instance FromJSON TestModel where
  -- parseJSON  = withObject (fromString "TestModel") $ \v -> TestModel <$> v .: fromString "first" <*> v .: fromString "second"
  parseJSON (Object v) = TestModel <$> v .: fromString "first" <*> v .: fromString "second"


[InstanceD Nothing [] (AppT (ConT Data.Aeson.Types.ToJSON.ToJSON) (ConT RunQ.TestModel)) 


[
  FunD Data.Aeson.Types.ToJSON.toJSON 
  [
    Clause 
      [VarP m_0] 
      (NormalB 
        (AppE 
          (VarE Data.Aeson.Types.Internal.object) 
          (ListE 
            [
              InfixE (Just (AppE (VarE Data.String.fromString) (LitE (StringL "first")))) (VarE Data.Aeson.Types.ToJSON..=) (Just (AppE (VarE RunQ.first) (VarE m_0))),
              InfixE (Just (AppE (VarE Data.String.fromString) (LitE (StringL "second")))) (VarE Data.Aeson.Types.ToJSON..=) (Just (AppE (VarE RunQ.second) (VarE m_0)))
            ]))) []]
  ]
]


-}
