{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 17 21:05:51 2003>
   License:    GPL
-}

{- DESCRIPTION

Exercise 3 - Using the List monad

Write functions parent and grandparent with signature Sheep ->
[Sheep]. They should return all sheep matching the description, or the
empty list if there is no such sheep. Hint: the mplus operator in the
List monad is useful here, as is the maybeToList function in the
module Maybe

-}

import Monad
import Maybe

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- Here we use the zero and plus aspects of the Maybe and List monads
-- to combine monadic values.

parent :: Sheep -> [Sheep]
parent s = (maybeToList (mother s)) `mplus` (maybeToList (father s))

grandparent :: Sheep -> [Sheep]
grandparent s = do p <- parent s
                   parent p

-- Why can we use a simpler definition of grandparent when using
-- the List monad than we could when using the Maybe monad?
-- ANSWER: Because the List monad's combination strategy effectively
-- implements backtracking but the Maybe monad's strategy does not.

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

-- print all of Dolly's grandparents
main :: IO ()
main = let dolly = breedSheep
       in do print (grandparent dolly)
	
-- END OF FILE
