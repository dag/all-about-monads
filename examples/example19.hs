{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:20:18 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 19 - Nesting the Continuation monad within the I/O monad

Usage: Compile the code and run it.  Type an integer value
       and press enter, and the program will print out an answer
       string whose format and contents depend in a complicated
       way on the input number.

Try: echo "0"         | ./ex19 
     echo "7"         | ./ex19 
     echo "9"         | ./ex19 
     echo "10"        | ./ex19 
     echo "19"        | ./ex19 
     echo "20"        | ./ex19 
     echo "68"        | ./ex19 
     echo "199"       | ./ex19 
     echo "200"       | ./ex19 
     echo "684"       | ./ex19 
     echo "19999"     | ./ex19 
     echo "20000"     | ./ex19 
     echo "20002"     | ./ex19 
     echo "340000"    | ./ex19 
     echo "837364"    | ./ex19 
     echo "1999997"   | ./ex19 
     echo "1999999"   | ./ex19 
     echo "2000000"   | ./ex19 
     echo "2000001"   | ./ex19 
     echo "2000002"   | ./ex19 
     echo "2000003"   | ./ex19 
     echo "2000004"   | ./ex19 
     echo "7001001"   | ./ex19 
     echo "746392736" | ./ex19 
-}

import IO
import Monad
import System
import Char
import Control.Monad.Cont

{- We use the continuation monad to perform "escapes" from code blocks.
   This function implements a complicated control structure to process
   numbers:

   Input (n)       Output                       List Shown
   =========       ======                       ==========
   0-9             n                            none
   10-199          number of digits in (n/2)    digits of (n/2)
   200-19999       n                            digits of (n/2)
   20000-1999999   (n/2) backwards              none
   >= 2000000      sum of digits of (n/2)       digits of (n/2)
-}
fun :: IO String
fun = do n <- (readLn::IO Int)         -- this is an IO monad block
         return $ (`runCont` id) $ do  -- this is a Cont monad block
           str <- callCC $ \exit1 -> do
             when (n < 10) (exit1 (show n))
             let ns = map digitToInt (show (n `div` 2))
             n' <- callCC $ \exit2 -> do
               when ((length ns) < 3) (exit2 (length ns))
               when ((length ns) < 5) (exit2 n)
               when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                           exit1 (dropWhile (=='0') ns')
               return $ sum ns
             return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
           return $ "Answer: " ++ str

-- calls fun with the integer read from the first command-line argument
main :: IO ()
main = do str <- fun
          putStrLn str

-- END OF FILE
