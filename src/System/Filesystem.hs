
-----------------------------------------------------------------------------------------------------------------------

module System.Filesystem
(
   listDir,
   textFindInFile,
   Result(..),
   printResult,
   printCompare,
   compDirs
) where


import qualified Data.ByteString as B
import Control.Monad.Trans.Resource(runResourceT, allocate, release)
import Control.Monad.Trans.Class(lift)
import Data.ByteString.Search(breakOn)
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist, canonicalizePath, getModificationTime)
import System.FilePath((</>), takeFileName)
import Control.Monad(forM, forM_, filterM, when)
import System.IO(hFileSize, openFile, hClose, IOMode(ReadMode), hIsEOF, hSeek, SeekMode(RelativeSeek))
import Text.Printf(printf)
import Data.List(intersect, (\\))
import Control.Exception.Base(bracket)

-----------------------------------------------------------------------------------------------------------------------

listDir :: FilePath -> (FilePath -> Bool) -> Bool -> IO [FilePath]
listDir topdir fileSelect recursive = do
   names <- getDirectoryContents topdir
   let properNames = filter (`notElem` [".", ".."]) names
   paths <- forM properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory && recursive
         then listDir path fileSelect True
         else
            if fileSelect path
               then return [path]
               else return []
   return (concat paths)

------------------------------------------------------------------------------------------------------------------------

textFindInFile :: B.ByteString -> Int -> FilePath -> IO Bool 
textFindInFile text bufferSize fileName = runResourceT $ do
   let
      textSize = B.length text
      propBufferSize = ((bufferSize `div` textSize) + 2) * textSize
      
   (fileReleaser, handle) <- allocate (openFile fileName ReadMode) hClose
   res <- lift $ find handle textSize propBufferSize
   release fileReleaser 
   return res
   
      where
         find handle textSize propBufferSize = do
            buffer <- B.hGetSome handle propBufferSize
            let (_, res) = breakOn text buffer
            if B.null res
               then do
                  isEof <- hIsEOF handle
                  if isEof
                     then return False
                     else do
                        hSeek handle RelativeSeek $ toInteger (- textSize)
                        find handle textSize propBufferSize
               else
                  return True
                  
------------------------------------------------------------------------------------------------------------------------                  
   
data Result 
   = DirResult
      {
         drSrcDir :: FilePath,
         drDestDir :: FilePath
      }
   | SrcDirResult
      {
         sdrDir :: FilePath
      }
   | DestDirResult
      {
         ddrDir :: FilePath
      }
   | FileResult
      {
         frSrcFile :: FilePath,
         frDestFile :: FilePath
      }
   | SrcFileResult
      {
         sfrFile :: FilePath
      }    
   | DestFileResult
      {
         dfrFile :: FilePath
      }
   | SrcDirFileMismatch
      {
         srcDir :: FilePath,
         destFile :: FilePath
      }
   | DestDirFileMismatch
      {
         destDir :: FilePath,
         srcFile :: FilePath
      }
   deriving(Show)


printResult :: Result -> String
printResult (DirResult sDir dDir) = printf  "<=> '%s' '%s'" sDir dDir
printResult (SrcDirResult sDir) = printf    "=>  '%s'" sDir
printResult (DestDirResult dDir) = printf   "<=  '%s' " dDir
printResult (FileResult sFile _) = printf   "=    %s" $ takeFileName sFile
printResult (SrcFileResult sFile) = printf  "=>   %s" $ takeFileName sFile
printResult (DestFileResult dFile) = printf "<=   %s" $ takeFileName dFile
printResult (SrcDirFileMismatch sDir dFile) = printf "File directory mismatch '%s' '%s'" sDir dFile
printResult (DestDirFileMismatch dDir sFile) = printf "File directory mismatch '%s' '%s'" dDir sFile


printCompare :: Result -> IO String
printCompare (DirResult sDir dDir) = do
   sc <- canonicalizePath sDir
   dc <- canonicalizePath dDir
   return $ printResult $ DirResult sc dc
   
printCompare (SrcDirResult dir) = do
   cd <- canonicalizePath dir
   return $ printResult $ SrcDirResult cd
   
printCompare (DestDirResult dir) = do
   cd <- canonicalizePath dir
   return $ printResult $ DestDirResult cd
   
printCompare (FileResult sFile dFile) = do
   srcModTime <- getModificationTime sFile
   destModTime <- getModificationTime dFile
   case compare srcModTime destModTime of
      LT -> return $ printf "<    %s" $ takeFileName sFile  
      GT -> return $ printf ">    %s" $ takeFileName sFile
      _  -> do
         srcFileSize <- getFileSize sFile
         destFileSize <- getFileSize dFile
         if srcFileSize == destFileSize
            then return $ printf "==   %s" $ takeFileName sFile
            else return $ printf "<>   %s" $ takeFileName sFile  
   
printCompare res = return $ printResult res 
   


compDirs :: Bool -> FilePath -> FilePath -> (Result -> IO ()) -> IO ()
compDirs recursive sDir' dDir' iterFun = do
   isSrcDir <- doesDirectoryExist sDir'
   isSrcFile <- doesFileExist sDir'
   isDestDir <- doesDirectoryExist dDir'
   isDestFile <- doesFileExist dDir'    
   
   case (isSrcDir, isSrcFile, isDestDir, isDestFile) of
      (True, False, True, False)  -> do
         iterFun $ DirResult sDir' dDir'  
         compDirsIter_ recursive sDir' dDir' iterFun
         
      (True, False, False, False) -> compOnlyDirIter recursive True sDir' iterFun 
      (False, False, True, False) -> compOnlyDirIter recursive False dDir' iterFun
      (True, False, False, True)  -> iterFun $ SrcDirFileMismatch sDir' dDir'
      (False, True, True, False)  -> iterFun $ DestDirFileMismatch dDir' sDir'
      _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
   where
      
      compDirsIter_ :: Bool -> FilePath -> FilePath -> (Result -> IO ()) -> IO ()
      compDirsIter_ recursive' sDir dDir iterFun' = do
         srcDirConts <- getDirectoryContents sDir
         destDirConts <- getDirectoryContents dDir
         let 
            propFile file = file /= "." && file /= ".."
            propSrcDirConts = filter propFile srcDirConts
            propDestDirConts = filter propFile destDirConts
            intersectConts = propSrcDirConts `intersect` propDestDirConts
            onlySrcConts = propSrcDirConts \\ propDestDirConts
            onlyDestConts = propDestDirConts \\ propSrcDirConts
            
         compIntersectResults intersectConts
         compOnlyResults True onlySrcConts  
         compOnlyResults False onlyDestConts
         
         where
            compOnlyResults :: Bool -> [FilePath] -> IO ()
            compOnlyResults isSrc conts = forM_ conts $ \name -> do             
               let
                  dir = if isSrc then sDir else dDir 
                  fullName = dir </> name   
                  
               isFile <- doesFileExist fullName
               isDir <- doesDirectoryExist fullName
               
               case (isFile, isDir) of
                  (True, False) -> iterFun' $ if isSrc then SrcFileResult fullName else DestFileResult fullName
                     
                  (False, True) -> do
                     iterFun' $ if isSrc then SrcDirResult fullName else DestDirResult fullName
                     when recursive' $ compOnlyDirIter True isSrc fullName iterFun'

                  (False, False) -> return ()
                  
                  _ -> error "Fatal error: The files system entry can be either a file or a directory."         
         
         
            compIntersectResults :: [FilePath] -> IO ()
            compIntersectResults intersectConts = forM_ intersectConts $ \name -> do             
               let 
                  srcFullName = sDir </> name   
                  destFullName = dDir </> name
                  
               isSrcFile <- doesFileExist srcFullName
               isSrcDir <- doesDirectoryExist srcFullName
               isDestFile <- doesFileExist destFullName
               isDestDir <- doesDirectoryExist destFullName
               
               case (isSrcFile, isSrcDir, isDestFile, isDestDir) of
                  (True, False, True, False) -> iterFun $ FileResult srcFullName destFullName
                  
                  (False, True, False, True) -> do
                     iterFun $ DirResult srcFullName destFullName
                     when recursive $ compDirsIter_ True srcFullName destFullName iterFun
                  
                  (False, True, True, False)  -> iterFun $ SrcDirFileMismatch srcFullName destFullName
                  
                  (True, False, False, True)  -> iterFun $ DestDirFileMismatch destFullName srcFullName
                     
                  _ -> error "Fatal error: The files system entry can be either a file or a directory."
      
      
      compOnlyDirIter :: Bool -> Bool -> FilePath -> (Result -> IO ()) -> IO ()
      compOnlyDirIter recursive' isSrc compDir iterFun' = do
         dirConts <- getDirectoryContents compDir
         let 
            propFile file = file /= "." && file /= ".."
            propDirConts = map (compDir </>) . filter propFile $ dirConts
            
         files <- filterM doesFileExist propDirConts
         forM_ files $ \file -> 
            let 
               rs = if isSrc
                  then SrcFileResult file
                  else DestFileResult file
            in
               iterFun' rs
                
         dirs <- filterM doesDirectoryExist propDirConts
         
         forM_ dirs $ \ dir ->
            let 
               rs = if isSrc
                  then SrcDirResult dir
                  else DestDirResult dir
            in do
               iterFun' rs
               when recursive' $ compOnlyDirIter True isSrc dir iterFun'

getFileSize :: FilePath -> IO Integer
getFileSize path = bracket (openFile path ReadMode) hClose hFileSize

-----------------------------------------------------------------------------------------------------------------------


