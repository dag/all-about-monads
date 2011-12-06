{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:26:33 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 20 - Lifting the IO monad through the Continuation monad
             without using the ContT monad transformer.

Usage: Compile the code and run it.  Type an integer value
       and press enter, and the program will print out an answer
       string whose format and contents depend in a complicated
       way on the input number.  It may ask you for another
       number.

Try: echo "0"            | ./ex20 
     echo "7"            | ./ex20 
     echo "9"            | ./ex20 
     echo "10"           | ./ex20 
     echo "19"           | ./ex20 
     echo "20"           | ./ex20 
     echo "68"           | ./ex20 
     echo "199"          | ./ex20 
     echo -e "200\n37"   | ./ex20 
     echo -e "684\n9483" | ./ex20 
     echo -e "19999\n0"  | ./ex20 
     echo "20000"        | ./ex20 
     echo "20002"        | ./ex20 
     echo "340000"       | ./ex20 
     echo "837364"       | ./ex20 
     echo "1999997"      | ./ex20 
     echo "1999999"      | ./ex20 
     echo "2000000"      | ./ex20 
     echo "2000001"      | ./ex20 
     echo "2000002"      | ./ex20 
     echo "2000003"      | ./ex20 
     echo "2000004"      | ./ex20 
     echo "7001001"      | ./ex20 
     echo "746392736"    | ./ex20 
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
toIO :: a -> IO a
toIO x = return x

fun :: IO String
fun = do n <- (readLn::IO Int)         -- this is an IO monad block
         convert n
	 
convert :: Int -> IO String
convert n = (`runCont` id) $ do        -- this is a Cont monad block
              str <- callCC $ \exit1 -> do    -- str has type IO String
                when (n < 10) (exit1 $ toIO (show n))
                let ns = map digitToInt (show (n `div` 2))
                n' <- callCC $ \exit2 -> do   -- n' has type IO Int
                  when ((length ns) < 3) (exit2 (toIO (length ns)))
                  when ((length ns) < 5) (exit2 $ do putStrLn "Enter a number:"
	                                             x <- (readLn::IO Int)
						     return x)
                  when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                              exit1 $ toIO (dropWhile (=='0') ns')
                  return (toIO (sum ns))
                return $ do num <- n'  -- this is an IO monad block
                            return $ "(ns = " ++ (show ns) ++ ") " ++ (show num)
              return $ do s <- str -- this is an IO monad block
                          return $ "Answer: " ++ s

-- calls fun, which does IO and computes a string value
main :: IO ()
main = do str <- fun
          putStrLn str

-- END OF FILE
