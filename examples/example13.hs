{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Wed Jul  2 18:11:36 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 13 - Using the List monad

Usage: Compile the code and execute the resulting program
       with various arguments.  Each argument will produce
       a list of possible parses of the argument as a decimal
       number, a hexadecimal number or a word.

Try: ./ex13 34 f3 bean
     ./ex13 food f00d
     ./ex13 beef 10 "n!"
     ./ex13 "why?" "(punctuation)"

-}

import Monad
import System
import Char

-- we can parse three different types of terms
data Parsed = Digit Integer | Hex Integer | Word String deriving Show

-- attempts to add a character to the parsed representation of a hex digit
parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex n) c = if isHexDigit c then
                            return (Hex ((n*16) + (toInteger (digitToInt c))))
		          else
                            mzero
parseHexDigit _       _ = mzero

-- attempts to add a character to the parsed representation of a decimal digit
parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit n) c = if isDigit c then
                           return (Digit ((n*10) + (toInteger (digitToInt c))))
                         else
                           mzero
parseDigit _         _ = mzero
		   
-- attempts to add a character to the parsed representation of a word
parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c = if isAlpha c then
                         return (Word (s ++ [c]))
                       else
                         mzero
parseWord _        _ = mzero

-- tries to parse the digit as a hex value, a decimal value and a word
-- the result is a list of possible parses
parse :: Parsed -> Char -> [Parsed]
parse p c = (parseHexDigit p c) `mplus` (parseDigit p c) `mplus` (parseWord p c)

-- parse an entire String and return a list of the possible parsed values
parseArg :: String -> [Parsed]
parseArg s = do init <- (return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))
                foldM parse init s

-- show the original string and all possible parses for the string
showResult :: String -> IO ()
showResult s = do putStr s
                  putStr ": "
                  print (parseArg s)

-- prints possible parsed values for command-line arguments
main :: IO ()
main = do args <- getArgs
          mapM_ showResult args

-- END OF FILE
