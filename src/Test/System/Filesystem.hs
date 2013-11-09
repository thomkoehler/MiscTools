------------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.System.Filesystem(htf_thisModulesTests) where

import Test.Framework

import System.Filesystem(textFindInFile)
import System.IO.Temp(withTempFile)
import System.IO(hPutStr, hClose)
import Data.ByteString.Char8(pack)
import Control.Monad(forM_)

------------------------------------------------------------------------------------------------------------------------

testDefs = 
   [
      ("Hallo", "Hallo", True),
      (" Hallo", "Hallo", True),
      ("  Hallo", "Hallo", True),
      ("   Hallo", "Hallo", True),
      ("    Hallo", "Hallo", True),
      ("     Hallo", "Hallo", True),
      ("      Hallo", "Hallo", True),
      ("       Hallo", "Hallo", True),
      ("        Hallo", "Hallo", True),
      ("         Hallo", "Hallo", True)
   ]

test_FindFile = forM_ testDefs testFindFile


testFindFile :: (String, String, Bool) -> IO ()
testFindFile (content, findStr, mustFound) = withTempFile "." "testFindFile.txt" $ \fileName hFile -> do
   hPutStr hFile content
   hClose hFile
   found <- textFindInFile (pack findStr) 1024 fileName
   assertEqual mustFound found


------------------------------------------------------------------------------------------------------------------------

