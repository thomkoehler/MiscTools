
-----------------------------------------------------------------------------------------------------------------------

module System.Filesystem
(
   listDir
) where


import System.Directory(getDirectoryContents, doesDirectoryExist)
import System.FilePath((</>), takeFileName)
import Control.Monad(forM)

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

-----------------------------------------------------------------------------------------------------------------------


