{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Wed Aug 13 10:58:14 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 6 - Using the lifting functions

Usage: Compile the code and execute the resulting program
       with a name as the argument.  If the name is in the
       database, it will retrieve the full name and print
       it in first+last order.

Try: ./ex6 "John"
     ./ex6 "Mike"
     ./ex6 "Fred"
-}

import Monad
import System
import Char (isSpace)

-- swapNames converts a string like "Smith, John" into "John Smith"
swapNames :: String -> String
swapNames s = let (ln,fn) = break (==',') s
              in (dropWhile isSpace (tail fn)) ++ " " ++ ln

-- getName looks up the name in the database and returns the
-- name in first+last order.
-- It uses the liftM operator to convert swapName from operating
-- on Strings to operating on Maybe Strings.
getName :: String -> Maybe String
getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  liftM swapNames (lookup name db)

{- Without using the liftM operation, we would have had to do something
   that is less succinct, like this:

getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  tempName <- lookup name db
	          return (swapNames tempName)
-}

-- This looks up the person and prints the name first then last.
-- it uses the lift operation to lift the swapNames function into
-- the Maybe monad, so that it can operate on Maybe String values.
main :: IO ()
main = do args <- getArgs
          case getName (args!!0) of
            Just n  -> print n
            Nothing -> putStrLn "No such person in the database"

-- END OF FILE