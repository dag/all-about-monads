{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 17 20:48:07 2003>
   License:    GPL
-}

{- DESCRIPTION

Exercise 2 - Combining Monadic Values

Write functions parent and grandparent with signature Sheep -> Maybe
Sheep. They should return one sheep selected from all sheep matching
the description, or Nothing if there is no such sheep. Hint: the mplus
operator is useful here.

-}

import Monad

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- we can use do-notation to build complicated sequences
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do m <- mother s
                           father m

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
				  mother gm

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
				  father gf

-- here are the new functions
parent :: Sheep -> Maybe Sheep
parent s = (mother s) `mplus` (father s)

grandparent :: Sheep -> Maybe Sheep
grandparent s = (mother s >>= parent) `mplus` (father s >>= parent)
		   
-- Why couldn't we write:
-- grandparent s = do p <- parent s
--                    parent p
-- Hint: What if a sheep's mother had no parents but its father did,
--       like Roger below?

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


-- print one of Dolly's grandparents
main :: IO ()
main = let dolly = breedSheep
       in do print (grandparent dolly)
	
-- END OF FILE
