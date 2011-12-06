{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Wed Jul  2 16:34:28 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 11 - Using the Maybe monad

Usage: Compile the code and execute the resulting program
       with an argument that is the name or nickname of
       a mail user in the system below:
         "Bill Gates"      "billy"
	 "Bill Clinton"    "slick willy"
	 "Michael Jackson" "jacko"
       That user's email preference will be printed as either
       "Just HTML" or "Just Plain".  An unrecognized name will generate
       output of "Nothing".

Try: ./ex11 "Bill Clinton"
     ./ex11 jacko
     ./ex11 Madonna
-}

import Monad
import System

-- this our super-simple email preference system
type EmailAddr = String
data MailPref = HTML | Plain deriving Show

data MailSystem = MS { fullNameDB::[(String,EmailAddr)],
                       nickNameDB::[(String,EmailAddr)],
		       prefsDB   ::[(EmailAddr,MailPref)] }

-- this is a more convenient way to specify user information
data UserInfo = User { name::String,
                       nick::String,
		       email::EmailAddr,
		       prefs::MailPref }

-- makeMailSystem will make a MailSystem from a list of users
makeMailSystem :: [UserInfo] -> MailSystem
makeMailSystem users = let fullLst = map (\u -> (name u, email u))  users
                           nickLst = map (\u -> (nick u, email u))  users
			   prefLst = map (\u -> (email u, prefs u)) users
		       in MS fullLst nickLst prefLst

-- getMailPrefs returns the email preference for a given user,
-- or Nothing if the user is not in the system.
getMailPrefs :: MailSystem -> String -> Maybe MailPref
getMailPrefs sys name =
  do let nameDB = fullNameDB sys
         nickDB = nickNameDB sys
         prefDB = prefsDB sys
     addr <- (lookup name nameDB) `mplus` (lookup name nickDB)
     lookup addr prefDB

-- print the email preference of the person named on the command-line
main :: IO ()
main = do let users = [ User "Bill Gates"      "billy"       "billg@microsoft.com" HTML,
                        User "Bill Clinton"    "slick willy" "bill@hope.ar.us"     Plain,
			User "Michael Jackson" "jacko"       "mj@wonderland.org"   HTML ]
	      mailsys = makeMailSystem users
	  args <- getArgs
          print (getMailPrefs mailsys (args!!0))
	  
-- END OF FILE