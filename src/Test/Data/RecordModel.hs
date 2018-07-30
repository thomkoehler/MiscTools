{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Data.RecordModel(htf_thisModulesTests) where

import Test.Framework
import Data.Aeson
import Data.String
import GHC.Generics

import Data.RecordModel.QQ


[model|

TestModel
  first Int
  second String
  deriving Eq Ord Show

|]


-- instance ToJSON TestModel where
--   toJSON m = object [fromString "first" .= first m, fromString "second" .= second m] 


instance FromJSON TestModel where
  --parseJSON  = withObject (fromString "TestModel") $ \v -> TestModel <$> v .: fromString "first" <*> v .: fromString "second"
  parseJSON (Object v) = TestModel <$> v .: fromString "first" <*> v .: fromString "second"


prop_testModel :: Bool
prop_testModel =
  let
    tm = TestModel 1 "Hello"
    txt = show tm
  in
    first tm == 1 && second tm == "Hello"

