{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Fri Aug 15 17:54:59 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 8 - Using the ap function

Usage: Compile the code and execute the resulting program.
       The first argument is an integer, the second argument is
       a space-delimited list of commands to apply to the number.
       
       The program will print a result value, or Nothing if any
       command is unknown.

Try: ./ex8 7  'double'                   
     ./ex8 10 'halve square negate'      
     ./ex8 0  'incr double square'       
     ./ex8 2  'square square square decr'
     ./ex8 45 'halve cube negate'
-}

import System
import Monad

-- lookup the commands and fold ap into the command list to
-- compute a result.
main :: IO ()
main = do let fns  = [("double",(2*)),      ("halve",(`div`2)),
                      ("square",(\x->x*x)), ("negate", negate),
                      ("incr",(+1)),        ("decr",(+(-1)))
                     ]
          args <- getArgs
          let val  = read (args!!0)
              cmds = map ((flip lookup) fns) (words (args!!1))
          print $ foldl (flip ap) (Just val) cmds

-- END OF FILE