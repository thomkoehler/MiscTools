{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  ScopedTypeVariables #-}

module Test.Data.RecordModel(htf_thisModulesTests) where

import Test.Framework
import Data.RecordModel.QQ

[model|

TestModel
  first Int
  second String
  deriving Eq Ord Show

|]

prop_testModel :: Bool
prop_testModel =
  let
    tm = TestModel 1 "Hello"
    txt = show tm
  in
    first tm == 1 && second tm == "Hello"

