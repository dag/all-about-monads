{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:18:22 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 18 - Using the Continuation monad

Usage: Compile the code and run it.  Give an integer argument,
       and the program will print out an answer string whose
       format and contents depend in a complicated way on the
       input number.

Try: ./ex18 "0"
     ./ex18 "7"
     ./ex18 "9"
     ./ex18 "10"
     ./ex18 "19"
     ./ex18 "20"
     ./ex18 "68"
     ./ex18 "199"
     ./ex18 "200"
     ./ex18 "684"
     ./ex18 "19999"
     ./ex18 "20000"
     ./ex18 "20002"
     ./ex18 "340000"
     ./ex18 "837364"
     ./ex18 "1999997"
     ./ex18 "1999999"
     ./ex18 "2000000"
     ./ex18 "2000001"
     ./ex18 "2000002"
     ./ex18 "2000003"
     ./ex18 "2000004"
     ./ex18 "7001001"
     ./ex18 "746392736"
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
fun :: Int -> String
fun n = (`runCont` id) $ do
        str <- callCC $ \exit1 -> do     -- define "exit1"
          when (n < 10) (exit1 (show n))
          let ns = map digitToInt (show (n `div` 2))
          n' <- callCC $ \exit2 -> do    -- define "exit2"
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) (exit2 n)
            when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                        exit1 (dropWhile (=='0') ns')  --escape 2 levels
            return $ sum ns
          return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
        return $ "Answer: " ++ str

-- calls fun with the integer read from the first command-line argument
main :: IO ()
main = do args <- getArgs
          let n = read (args!!0)
          putStrLn (fun n)

-- END OF FILE
