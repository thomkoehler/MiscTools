-----------------------------------------------------------------------------------------------------------------------

module Main
(
   main
)
where

-----------------------------------------------------------------------------------------------------------------------

import System.Exit (exitFailure)
import Control.Monad (unless)
import GHC.List(and)

import qualified Test.Data.Enum

-----------------------------------------------------------------------------------------------------------------------

allTests =
   [
      Test.Data.Enum.checkAll
   ]

main = do
   success <- sequenceAnd allTests
   unless success exitFailure
   where
      sequenceAnd actions = do
         rs <- sequence actions
         return $ and rs

-----------------------------------------------------------------------------------------------------------------------
