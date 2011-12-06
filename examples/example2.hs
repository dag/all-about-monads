{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 10 11:58:21 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 2 - Do notation

Usage: Compile the code and execute the resulting program.
       It will print Dolly's maternal grandfather.
-}

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- the Maybe type is already declared as an instance of the Monad class
-- in the standard prelude, so we don't actually need to define it here.
-- just remember that it looks something like this:
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing
--    (Just x) >>= f = f x
--    return         = Just

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
	