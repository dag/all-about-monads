{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:29:02 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 21 - Lifting the IO monad through the Continuation monad
             using the ContT monad transformer.

Usage: Compile the code and run it.  Type an integer value
       and press enter, and the program will print out an answer
       string whose format and contents depend in a complicated
       way on the input number.  It may ask you for another
       number.

Try: echo "0"            | ./ex21
     echo "7"            | ./ex21 
     echo "9"            | ./ex21 
     echo "10"           | ./ex21 
     echo "19"           | ./ex21 
     echo "20"           | ./ex21 
     echo "68"           | ./ex21 
     echo "199"          | ./ex21 
     echo -e "200\n37"   | ./ex21 
     echo -e "684\n9483" | ./ex21 
     echo -e "19999\n0"  | ./ex21 
     echo "20000"        | ./ex21 
     echo "20002"        | ./ex21 
     echo "340000"       | ./ex21 
     echo "837364"       | ./ex21 
     echo "1999997"      | ./ex21 
     echo "1999999"      | ./ex21 
     echo "2000000"      | ./ex21 
     echo "2000001"      | ./ex21 
     echo "2000002"      | ./ex21 
     echo "2000003"      | ./ex21 
     echo "2000004"      | ./ex21 
     echo "7001001"      | ./ex21 
     echo "746392736"    | ./ex21 
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
   200-19999       <ask the user>               digits of (n/2)
   20000-1999999   (n/2) backwards              none
   >= 2000000      sum of digits of (n/2)       digits of (n/2)
-}
fun :: IO String
fun = (`runContT` return) $ do
        n   <- liftIO (readLn::IO Int)
        str <- callCC $ \exit1 -> do     -- define "exit1"
          when (n < 10) (exit1 (show n))
          let ns = map digitToInt (show (n `div` 2))
          n' <- callCC $ \exit2 -> do    -- define "exit2"
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) $ do liftIO $ putStrLn "Enter a number:"
                                        x <- liftIO (readLn::IO Int)
                                        exit2 x
            when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                        exit1 (dropWhile (=='0') ns')  --escape 2 levels
            return $ sum ns
          return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
        return $ "Answer: " ++ str

-- calls fun, which does IO and computes a string value
main :: IO ()
main = do str <- fun
          putStrLn str

-- END OF FILE
