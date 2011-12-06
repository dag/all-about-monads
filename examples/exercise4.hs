{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 17 21:17:02 2003>
   License:    GPL
-}

{- DESCRIPTION

Exercise 4 - Using the Monad class constraint

Write functions parent and grandparent with signature
(MonadPlus m) => Sheep -> m Sheep. They should be useful in both the
Maybe and List monads. How does the functions' behavior differ when
used with the List monad versus the Maybe monad?

-}

import Monad
import Maybe

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- convert a Maybe value into another monad
maybeToMonad :: (MonadPlus m) => Maybe a -> m a
maybeToMonad Nothing  = mzero
maybeToMonad (Just s) = return s

-- Here we use the zero and plus aspects of the Maybe and List monads
-- to combine monadic values.

parent :: (MonadPlus m) => Sheep -> m Sheep
parent s = (maybeToMonad (mother s)) `mplus` (maybeToMonad (father s))

grandparent :: (MonadPlus m) => Sheep -> m Sheep
grandparent s = (maybeToMonad (mother s) >>= parent) `mplus`
                (maybeToMonad (father s) >>= parent)

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

-- print Dolly's grandparents, once in the Maybe monad and then
-- again in the List monad
main :: IO ()
main = let dolly = breedSheep
       in do print ((grandparent dolly)::Maybe Sheep)
             print ((grandparent dolly)::[Sheep])
	
-- END OF FILE
