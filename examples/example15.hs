{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Thu Jul 24 13:39:30 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 15 - Using the State monad

Usage: Compile the code and execute the command.
       It will print two identical random values of type MyType.
       The first value is computed without the State monad and
       the second is computed using the State monad.
       The MyType values are random but obey some internal
       invariants:
         o  the Int value is in the range 1-100.
         o  the Char value is in the range 'a'-'z'
	 o  the absolute value of the second Int value is
	    less than or equal to the value of the first Int value
	    
Try: ./ex15
-}

import Monad
import System
import IO
import Random
import Control.Monad.State

-- This is the type that we want to generate random values of
data MyType = MT Int Bool Char Int deriving Show

{- Without using the State monad, we would have to thread the
   random number generator state by hand.  The function would
   look like this:
-}
makeRandomValue :: StdGen -> (MyType, StdGen)
makeRandomValue g = let (n,g1) = randomR (1,100) g
                        (b,g2) = random g1
                        (c,g3) = randomR ('a','z') g2 
                        (m,g4) = randomR (-n,n) g3
		    in (MT n b c m, g4)

{- Using the State monad, we can define a function that returns
   a random value and updates the random generator state at
   the same time.
-}

getAny :: (Random a) => State StdGen a
getAny = do g      <- get
            (x,g') <- return $ random g
	    put g'
	    return x

-- similar to getAny, but it bounds the random value returned
getOne :: (Random a) => (a,a) -> State StdGen a
getOne bounds = do g      <- get
                   (x,g') <- return $ randomR bounds g
                   put g'
                   return x

{- Using the State monad with StdGen as the state, we can build
   random complex types without manually threading the
   random generator states through the code.
-}   
makeRandomValueST :: StdGen -> (MyType, StdGen)
makeRandomValueST = runState (do n <- getOne (1,100)
                                 b <- getAny
                                 c <- getOne ('a','z')
                                 m <- getOne (-n,n)
                                 return (MT n b c m))

-- print a random value of MyType, showing the two implementations
-- are equivalent
main :: IO ()
main = do g <- getStdGen
          print $ fst $ makeRandomValue g
          print $ fst $ makeRandomValueST g

-- END OF FILE
