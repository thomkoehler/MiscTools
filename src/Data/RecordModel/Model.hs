module Data.RecordModel.Model where

import Language.Haskell.TH

data Field = Field
  {
    fieldName     :: !String,
    fieldJsonName :: !String,
    fieldType     :: !Type
  }


data Model = Model
  {
    modelName      :: !String,
    modelFields    :: [Field],
    modelDerivings :: [String]
  }

