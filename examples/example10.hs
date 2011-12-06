{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Fri Aug 15 08:40:08 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 10 - Using the guard function

Usage: Compile the code and execute the resulting program
       with an integer as its argument.  It will print the
       records for everyone in the database younger than
       the specified age.
       
Try: ./ex10 21
     ./ex10 40
     ./ex10 38
     ./ex10 10
     ./ex10 2
-}

import System
import Maybe
import Monad

-- we have records containing name and age
data Record = Rec {name::String, age::Int} deriving Show

-- we collect the records into a "database"
type DB = [Record]

-- getYoungerThan returns all records for people younger than a specified age.
-- It uses the guard function to eliminate records for ages at or over the limit.
-- This is just for demonstration purposes.  In real life, it would be
-- clearer to simply use filter.  When the filter criteria are more complex,
-- guard becomes more useful.
getYoungerThan :: Int -> DB -> [Record]
getYoungerThan limit db =
  mapMaybe (\r -> do { guard (age r < limit); return r }) db

-- list the records for people younger than the age given in args!!0
main :: IO ()
main = do let db = [Rec "Marge" 37, Rec "Homer" 38, Rec "Bart" 11, Rec "Lisa" 8, Rec "Maggie" 2]
          args <- getArgs
          print $ getYoungerThan (read (args!!0)) db

-- END OF FILE