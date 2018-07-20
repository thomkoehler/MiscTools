
module Data.RecordModel.QQ(model) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
createDec text = 
  let
    models = parse "" text
    dataDs = map modelToDataD models
  in
    return dataDs


modelToDataD :: Model -> Dec
modelToDataD (Model n fields derivs) = DataD [] (mkName n) [] Nothing [RecC (mkName n) (map fieldToVarStrictType fields)] [strToDerivClause derivs]

strToDerivClause :: [String] -> DerivClause
strToDerivClause names = DerivClause Nothing $ map (ConT . mkName) names

fieldToVarStrictType :: Field -> VarStrictType
fieldToVarStrictType (Field n t) = (mkName n , Bang NoSourceUnpackedness NoSourceStrictness, t)

