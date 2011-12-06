{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 10 16:37:59 2003>
   License:    GPL
-}

{- DESCRIPTION

Exercise 1 - Do notation

Rewrite the maternalGrandfather, fathersMaternalGrandmother, and
mothersPaternalGrandfather functions in Example 2 using the monadic
operators return and >>=, without using any do-notation syntactic
sugar.

-}

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- a literal translation would be:
-- maternalGrandfather s = mother s >>= \m ->
--                         father m
-- but this equivalent function is more succinct:
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = mother s >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = father s >>= mother >>= mother

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = mother s >>= father >>= father

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

-- print Dolly's maternal grandfather
main :: IO ()
main = let dolly = breedSheep
       in do print (maternalGrandfather dolly)
	
-- END OF FILE
