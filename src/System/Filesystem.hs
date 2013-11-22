
-----------------------------------------------------------------------------------------------------------------------

module System.Filesystem
(
   listDir,
   textFindInFile
) where


import qualified Data.ByteString as B
import Control.Monad.Trans.Resource(runResourceT, allocate, release)
import Control.Monad.Trans.Class(lift)
import Data.ByteString.Search(breakOn)
import System.Directory(getDirectoryContents, doesDirectoryExist)
import System.FilePath((</>))
import Control.Monad(forM)
import System.IO(openFile, hClose, IOMode(ReadMode), hIsEOF, hSeek, SeekMode(RelativeSeek))

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
                  
-----------------------------------------------------------------------------------------------------------------------


