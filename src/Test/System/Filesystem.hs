------------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.System.Filesystem(htf_thisModulesTests) where

import Test.Framework

import System.Filesystem(textFindInFile)
import System.IO.Temp(withTempFile)
import System.IO(hPutStr, hClose)

------------------------------------------------------------------------------------------------------------------------

test_FindFile = do
   res <- testFindFile "Hallo"   
   assertEqual res True


testFindFile :: String -> IO Bool
testFindFile str = withTempFile "." "testFindFile.txt" $ \_ hFile -> do
   hPutStr hFile str
   hClose hFile
   return True


------------------------------------------------------------------------------------------------------------------------

