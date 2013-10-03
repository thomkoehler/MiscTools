
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------------------------------------------------

module Test.Data.Enum(checkAll) where

import Test.QuickCheck.All(quickCheckAll)
import Data.Enum.TH

------------------------------------------------------------------------------------------------------------------------

$(enumADT "TestEnum0" [("E0", 0), ("E1", 1), ("E2", 2), ("E3", 16777217)])

prop_fromE0 = fromEnum E0 == 0
prop_fromE1 = fromEnum E1 == 1
prop_fromE2 = fromEnum E2 == 2
prop_fromE3 = fromEnum E3 == 16777217

prop_toE0 = toEnum 0 == E0
prop_toE1 = toEnum 1 == E1
prop_toE2 = toEnum 2 == E2
prop_toE3 = toEnum 16777217 == E3

------------------------------------------------------------------------------------------------------------------------

$(enumADTDef "TestEnumDef0" "defTestEnumDef0" [("D0", 0), ("D1", 1)])
defTestEnumDef0 _ = D1

prop_fromDef0 = fromEnum D0 == 0
prop_fromDef1 = fromEnum D1 == 1

prop_toDef0 = toEnum 0 == D0
prop_toDef1 = toEnum 1 == D1
prop_toDef2 = toEnum 2 == D1

prop_fromDef i = case i of
   0 -> fromEnum D0 == 0
   otherwise -> toEnum i == D1

------------------------------------------------------------------------------------------------------------------------

checkAll = $quickCheckAll

------------------------------------------------------------------------------------------------------------------------

