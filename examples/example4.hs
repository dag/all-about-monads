{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Wed Jul  2 16:49:28 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 4 - Using the foldM function - part 2

Usage: Compile the code and execute the resulting program
       with names of definition files as arguments.
       The definition files have the form of a series
       of assignments "key = value", one per line.
       The assignments from each file are accumulated
       and then the final dictionary is printed
       as an association list.

-}

import Monad
import IO
import System
import Data.FiniteMap
import Char(isSpace)

-- an Entry is a key and a value, both Strings
data Entry = Entry {key::String, value::String}

-- show an entry as "key = value"
instance Show Entry where
  show e = show (key e) ++ " = " ++ (show (value e))

-- we parse "key = value" strings into Entry values
instance Read Entry where
  readsPrec _ s = readsEntry s

readsEntry :: ReadS Entry
readsEntry s = [(Entry (trim key) (trim val), s'') | (key, s')    <- [break (=='=') s],
                                                     (x:val, s'') <- [break (=='\n') s'] ]

-- remove leading and trailing whitespace
trim :: String -> String
trim s = dropWhile isSpace (reverse (dropWhile isSpace (reverse s)))

-- convenience function
openForReading :: FilePath -> IO Handle
openForReading f = openFile f ReadMode

-- a Dict is just a finite map from strings to strings
type Dict = FiniteMap String String

-- this an auxilliary function used with foldl
addEntry :: Dict -> Entry -> Dict
addEntry d e = addToFM d (key e) (value e)

-- this is an auxiliiary function used with foldM inside the IO monad
addDataFromFile :: Dict -> Handle -> IO Dict
addDataFromFile dict hdl = do contents <- hGetContents hdl
                              entries  <- return (map read (lines contents))
			      return (foldl (addEntry) dict entries)

-- this program builds a dictionary from the entries in all files named on the
-- command line and then prints it out as an association list
main :: IO ()
main = do files   <- getArgs
          handles <- mapM openForReading files
	  dict    <- foldM addDataFromFile emptyFM handles
	  print (fmToList dict)

-- END OF FILE