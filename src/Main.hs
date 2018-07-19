-----------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main
(
   main
)
where

import Test.Framework

-----------------------------------------------------------------------------------------------------------------------

import {-@ HTF_TESTS @-} Test.Data.Enum
import {-@ HTF_TESTS @-} Test.System.Filesystem
import {-@ HTF_TESTS @-} Test.Data.RecordModel

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = htfMain htf_importedTests

-----------------------------------------------------------------------------------------------------------------------
