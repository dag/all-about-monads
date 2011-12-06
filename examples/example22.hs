{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:49:10 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 22 - Using the WriterT monad transformer

Usage: Compile the code to produce a simple firewall simulator.

       The first argument is a file (look at rules.txt) that contains
       a list of firewall rules.
       
       The second argument is a file (look at packets.txt) that contains
       a list of packets.

       Running the program with the two arguments will cause the
       program to load the rule set and then use the rules to filter all
       of the packets in the packet file.  The program will display all
       of the allowed packets followed by a log of the simulated
       firewall activity.
       
Try: ./ex22 rules.txt packets.txt
-}

import IO
import Monad
import System
import Maybe
import List
import Time
import Control.Monad.Writer
import Control.Monad.Trans

-- this is the definition of our simple packet format
data Data   = AnyData | Data String deriving (Show,Read)
data Addr   = AnyHost | Host Int    deriving (Show,Read)
data Packet = Packet {from::Addr, to::Addr, payload::Data} deriving (Eq,Show,Read)

-- these Eq definitions allow for pattern matching
instance Eq Data where
  AnyData   == _         = True
  _         == AnyData   = True
  (Data s1) == (Data s2) = s1 == s2
  
instance Eq Addr where
  AnyHost   == _         = True
  _         == AnyHost   = True
  (Host h1) == (Host h2) = h1 == h2

-- this is the format of our rules
data Disposition = Accept | Reject deriving (Eq,Show,Read)
data Rule = Rule {disposition::Disposition, pattern::Packet, logIt::Bool} deriving (Eq,Show,Read)

-- match a packet against a single rule
matchPacket :: Packet -> Rule -> Maybe Rule
matchPacket packet rule = if pattern rule == packet then Just rule else Nothing

-- match a packet against a list of rules
match :: [Rule] -> Packet -> Maybe Rule
match rules packet = foldl mplus Nothing (map (matchPacket packet) rules)

-- this is the format of our log entries
data Entry = Log {timestamp::ClockTime, msg::String} deriving Eq

instance Show Entry where
  show (Log t s) = (show t) ++ " | " ++ s

-- this is the combined monad type
type LogWriter a = WriterT [Entry] IO a

-- add a message to the log
logMsg :: String -> LogWriter ()
logMsg s = do t <- liftIO getClockTime
              tell [Log t s]

-- this handles one packet
filterOne :: [Rule] -> Packet -> LogWriter (Maybe Packet)
filterOne rules packet = do rule <- return (match rules packet)
                            case rule of
                              Nothing  -> do logMsg ("DROPPING UNMATCHED PACKET: " ++ (show packet))
                                             return Nothing
                              (Just r) -> do when (logIt r) (logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet)))
                                             case r of
                                               (Rule Accept _ _) -> return (Just packet)
                                               (Rule Reject _ _) -> return Nothing

-- this filters a list of packets, producing a filtered packet list
-- and a log of the activity
filterAll :: [Rule] -> [Packet] -> LogWriter [Packet]
filterAll rules packets = do logMsg "STARTING PACKET FILTER"
                             out <- mapM (filterOne rules) packets
                             logMsg "STOPPING PACKET FILTER"
                             return (catMaybes out)

-- read the rule data from the file named in the first argument, and the packet data from
-- the file named in the second argument, and then print the accepted packets followed by
-- a log generated during the computation.
main :: IO ()
main = do args       <- getArgs
	  ruleData   <- readFile (args!!0)
	  packetData <- readFile (args!!1)
	  let rules   = (read ruleData)::[Rule]
	      packets = (read packetData)::[Packet]
	  (out,log)  <- runWriterT (filterAll rules packets)
	  putStrLn "ACCEPTED PACKETS"
	  putStr (unlines (map show out))
	  putStrLn "\n\nFIREWALL LOG"
	  putStr (unlines (map show log))

-- END OF FILE
