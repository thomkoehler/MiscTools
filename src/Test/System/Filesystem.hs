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
      ("ab", "ab", True, 2),
      (" ab", "ab", True, 2),
      ("  ab", "ab", True, 2),
      ("   ab", "ab", True, 2),
      ("    ab", "ab", True, 2),
      ("     ab", "ab", True, 2),
      ("      ab", "ab", True, 2),
      ("       ab", "ab", True, 2),
      ("        ab", "ab", True, 2),
      ("         ab", "ab", True, 2),
      (" Hallo ", "Hallo", True, 1024),
      (" Hxllo ", "Hallo", False, 1024),
      (" Haxlo ", "Hallo", False, 1024),
      (" Halxo ", "Hallo", False, 1024),
      (" Hallx ", "Hallo", False, 1024)
   ]

test_FindFile = forM_ testDefs testFindFile


testFindFile :: (String, String, Bool, Int) -> IO ()
testFindFile (content, findStr, mustFound, bufferSize) = withTempFile "." "testFindFile.txt" $ \fileName hFile -> do
   hPutStr hFile content
   hClose hFile
   found <- textFindInFile (pack findStr) bufferSize fileName
   assertEqual mustFound found


------------------------------------------------------------------------------------------------------------------------

