{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:47:36 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 17 - Using the Writer monad

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
       
Try: ./ex17 rules.txt packets.txt
-}

import IO
import Monad
import System
import Maybe
import List
import Control.Monad.Writer

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
data Entry = Log {count::Int, msg::String} deriving Eq

instance Show Entry where
  show (Log 1 s) = s
  show (Log n s) = (show n) ++ " X " ++ s

-- add a message to the log
logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

-- merge identical entries at the end of the log
-- This function uses [Entry] as both the log type and the result type.
-- When two identical messages are merged, the result is just the message
-- with an incremented count.  When two different messages are merged,
-- the first message is logged and the second is returned as the result.
mergeEntries :: [Entry] -> [Entry] -> Writer [Entry] [Entry]
mergeEntries []   x    = return x
mergeEntries x    []   = return x
mergeEntries [e1] [e2] = let (Log n msg)   = e1
                             (Log n' msg') = e2
                         in if msg == msg' then
                              return [(Log (n+n') msg)]
                            else
                              do tell [e1]
                                 return [e2]

-- this handles one packet
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do rule <- return (match rules packet)
                            case rule of
                              Nothing  -> do logMsg ("DROPPING UNMATCHED PACKET: " ++ (show packet))
                                             return Nothing
                              (Just r) -> do when (logIt r) (logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet)))
                                             case r of
                                               (Rule Accept _ _) -> return (Just packet)
                                               (Rule Reject _ _) -> return Nothing

-- This is a complex-looking function but it is actually pretty simple.
-- It maps a function over a list of values to get a list of Writers,
-- then runs each writer and combines the results.  The result of the function
-- is a writer whose value is a list of all the values from the writers and whose
-- log output is the result of folding the merge operator into the individual
-- log entries (using 'initial' as the initial log value).
groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial merge []     _  = do tell initial
                                       return []
groupSame initial merge (x:xs) fn = do (result,output) <- return (runWriter (fn x))
                                       new             <- merge initial output
                                       rest            <- groupSame new merge xs fn
                                       return (result:rest)
     
-- this filters a list of packets, producing a filtered packet list and a log of
-- the activity in which consecutive messages are merged
filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll rules packets = do tell [Log 1 "STARTING PACKET FILTER"]
                             out <- groupSame [] mergeEntries packets (filterOne rules)
                             tell [Log 1 "STOPPING PACKET FILTER"]
                             return (catMaybes out)

-- read the rule data from the file named in the first argument, and the packet data from
-- the file named in the second argument, and then print the accepted packets followed by
-- a log generated during the computation.
main :: IO ()
main = do args       <- getArgs
	  ruleData   <- readFile (args!!0)
	  packetData <- readFile (args!!1)
	  let rules     = (read ruleData)::[Rule]
	      packets   = (read packetData)::[Packet]
	      (out,log) = runWriter (filterAll rules packets)
	  putStrLn "ACCEPTED PACKETS"
	  putStr (unlines (map show out))
	  putStrLn "\n\nFIREWALL LOG"
	  putStr (unlines (map show log))

-- END OF FILE
