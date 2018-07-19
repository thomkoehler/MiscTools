
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
  in
    return $ map modelToDataD models


modelToDataD :: Model -> Dec
modelToDataD (Model n fields) = DataD [] (mkName n) [] Nothing [NormalC (mkName n) (map fieldToBangType fields)] []

fieldToBangType :: Field -> BangType
fieldToBangType (Field n t) = (Bang NoSourceUnpackedness NoSourceStrictness, t)