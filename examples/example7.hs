{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Wed Aug 13 11:20:22 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 7 - Using the lifting functions

Usage: Compile the code and execute the resulting program.
       It will print out a number of different lists and their
       combinations.
       
Try: ./ex7
-}

import Monad

-- allCombinations returns a list containing the result of
-- folding the binary operator through all combinations
-- of elements of the given lists
-- For example, allCombinations (+) [[0,1],[1,2,3]] would be
--   [0+1,0+2,0+3,1+1,1+2,1+3], or [1,2,3,2,3,4]
-- and allCombinations (*) [[0,1],[1,2],[3,5]] would be
--   [0*1*3,0*1*5,0*2*3,0*2*5,1*1*3,1*1*5,1*2*3,1*2*5], or [0,0,0,0,3,5,6,10]
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations fn []     = []
allCombinations fn (l:ls) = foldl (liftM2 fn) l ls

-- print an example
showExample :: (Show a) => String -> (a -> a -> a) -> [[a]] -> IO ()
showExample opName op ls = do putStrLn $ "opName over " ++ (show ls) ++ " = "
                              putStrLn $ "  " ++ (show (allCombinations op ls)) 

-- shows a few examples of using allCombinations
main :: IO ()
main = do showExample "+" (+)   [[0,1],[1,2,3]]
          showExample "*" (*)   [[0,1],[1,2],[3,5]]
          showExample "/" div   [[100, 45, 365], [3, 5], [2, 4], [2]]

-- END OF FILE