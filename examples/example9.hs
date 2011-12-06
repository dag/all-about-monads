{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Wed Aug 13 12:48:58 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 9 - Using the msum function

Usage: Compile the code and execute the resulting program.
       The first argument is a variable name, the second
       is an environment stack (list of String to String
       association lists).
       
Try: ./ex9 'depth' '[[("name","test"),("depth","2")], [("depth","1")]]'
     ./ex9 'width' '[[("name","test"),("depth","2")], [("depth","1")]]'
     ./ex9 'var3'  '[[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]'
     ./ex9 'var2'  '[[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]'
     ./ex9 'var1'  '[[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]'
     ./ex9 'var'   '[[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]'
-}

import System
import Monad

type Variable = String
type Value = String
type EnvironmentStack = [[(Variable,Value)]]

-- lookupVar retrieves a variable's value from the environment stack
-- It uses msum in the Maybe monad to return the first non-Nothing value.
lookupVar :: Variable -> EnvironmentStack -> Maybe Value
lookupVar var stack = msum $ map (lookup var) stack

{- Without using msum, we would have to do something like:
lookupVar :: Variable -> EnvironmentStack -> Maybe Value
lookupVar var []     = Nothing
lookupVar var (e:es) = let val = lookup var e
                       in maybe (lookupVar var es) Just val
--}

-- lookup the variable named in arg!!0 in the environment stack
-- given in arg!!1
main :: IO ()
main = do args <- getArgs
          print $ lookupVar (args!!0) (read (args!!1))

-- END OF FILE