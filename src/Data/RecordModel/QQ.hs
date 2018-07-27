
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.RecordModel.QQ(model) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Aeson

import Data.RecordModel.Parser
import Data.RecordModel.Model

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
  return $ dataDs ++ concat toJsonInstances


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
encodeField field = [| fromString $(varE (mkName (fieldName field))) .= first m |]


modelToJSONInstance :: Model -> Q [Dec]
modelToJSONInstance model' = do
  let encodeFields = map encodeField $ modelFields model'
  [d| instance ToJSON $(conT (mkName (modelName model'))) where toJSON m = object $(listE encodeFields) |]



{-

instance ToJSON TestModel where
  toJSON m = object [fromString "first" .= first m, fromString "second" .= second m] 



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