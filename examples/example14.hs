{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Thu Aug 14 09:53:53 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 14 - Using the IO monad

Usage: Compile the code to produce a poor replacement for
       the standard Unix "tr" command.

Try: cat file | ./ex14 "aeiou" "X"
     cat file | ./ex14 " " "_"
     ./ex14 "abc"
-}

import Monad
import System
import IO
import Control.Monad.Error

-- translate char in set1 to corresponding char in set2
translate :: String -> String -> Char -> Char
translate []     _      c = c
translate (x:xs) []     c = if x == c then ' ' else translate xs []  c
translate (x:xs) [y]    c = if x == c then  y  else translate xs [y] c
translate (x:xs) (y:ys) c = if x == c then  y  else translate xs ys  c

-- translate an entire string
translateString :: String -> String -> String -> String
translateString set1 set2 str = map (translate set1 set2) str

usage :: IOError -> IO ()
usage e = do putStrLn "Usage: ex14 set1 set2"
             putStrLn "Translates characters in set1 on stdin to the corresponding"
             putStrLn "characters from set2 and writes the translation to stdout."

-- translates stdin to stdout based on commandline arguments
main :: IO ()
main = (do [set1,set2] <- getArgs
           contents    <- hGetContents stdin
           putStr $ translateString set1 set2 contents)
       `catchError` usage

-- END OF FILE
