{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Wed Jul  2 16:53:12 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 5 - Using the filterM function

Usage: Compile the code and execute the resulting program
       with a number of file and directory names as arguments.
       The program will print only the names of the directories.
-}

import Monad
import Directory
import System

-- NOTE: doesDirectoryExist has type FilePath -> IO Bool

-- this program prints only the directories named on the command line
main :: IO ()
main = do names <- getArgs
          dirs  <- filterM doesDirectoryExist names
          mapM_ putStrLn dirs

-- END OF FILE