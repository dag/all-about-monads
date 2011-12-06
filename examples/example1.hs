{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 10 11:59:14 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 1 - Our first monad

Usage: Compile the code and execute the resulting program.
       It will print Dolly's maternal grandfather.
-}

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- comb is a combinator for sequencing operations that return Maybe
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing  _ = Nothing
comb (Just x) f = f x

-- now we can use `comb` to build complicated sequences
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = (Just s) `comb` mother `comb` father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = (Just s) `comb` father `comb` mother `comb` mother 

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = (Just s) `comb` mother `comb` father `comb` father 

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