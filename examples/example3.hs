{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 10 11:59:36 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 3 - Using the foldM function - part 1

Usage: Compile the code and execute the resulting program
       with arguments that describe an ancestor.
       The program will print the sheep that has that
       relationship with Dolly, or Nothing if there is
       no such sheep.

Try: ./ex3 mother
     ./ex3 father
     ./ex3 mother mother father
     ./ex3 father mother
-}

import Monad
import System

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- traceFamily is a generic function to find an ancestor
traceFamily :: Sheep -> [ (Sheep -> Maybe Sheep) ] -> Maybe Sheep
traceFamily s l = foldM getParent s l
  where getParent s f = f s

-- we can define complex queries using traceFamily in an easy, clear way
paternalGrandmother        s = traceFamily s [father, mother]
mothersPaternalGrandfather s = traceFamily s [mother, father, father]

-- this allows the user to name the mother and father functions on the command line
getFunctionByName :: String -> (Sheep -> Maybe Sheep)
getFunctionByName "father" = father
getFunctionByName "mother" = mother
getFunctionByName _        = error "Invalid function name - not 'mother' or 'father'"

-- this builds our sheep family tree
breedSheep :: Sheep
breedSheep = let adam   = Sheep "Adam" Nothing Nothing
                 eve    = Sheep "Eve" Nothing Nothing
		 uranus = Sheep "Uranus" Nothing Nothing
		 gaea   = Sheep "Gaea" Nothing Nothing
		 kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                 holly  = Sheep "Holly" (Just eve) (Just adam)
	         roger  = Sheep "Roger" (Just eve) (Just kronos)
	         molly  = Sheep "Molly" (Just holly) (Just roger)
	     in Sheep "Dolly" (Just molly) Nothing

-- we can use monadic operations to build complicated sequences
main :: IO ()
main = let dolly = breedSheep
       in do args <- getArgs
	     print $ traceFamily dolly (map getFunctionByName args)

-- END OF FILE
