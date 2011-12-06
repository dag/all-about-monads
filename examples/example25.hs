{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Tue Aug 19 09:31:32 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 25 - Using the StateT monad transformer
             with the List monad to achieve non-deterministic
             stateful computations, and the Writer monad to
             do logging

Usage: Compile the code and run it with an argument between 1 and 8.
       It will print a solution to the N-queens puzzle along with
       a log of the number of choices it had at each step.
       
       The N-queens puzzle is to place N queens on a chess board
       so that no queen can attack another one.
       
Try: ./ex25 8
     ./ex25 1
     ./ex25 7
-}

import IO
import Monad
import System
import Maybe
import List
import Char (toLower)
import Control.Monad.State
import Control.Monad.Writer

-- describe Chess pieces and positions

type Rank = Int

data File = A | B | C | D | E | F | G | H
  deriving (Eq, Show, Ord, Enum)

data Position = Pos {file::File, rank::Rank}
  deriving Eq

instance Show Position where
  show (Pos f r) = (map toLower (show f)) ++ (show r)

instance Ord Position where
  compare p1 p2 = case (rank p1) `compare` (rank p2) of
                    LT -> GT
		    GT -> LT
		    _  -> (file p1) `compare` (file p2)
		    
data Kind = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Enum)

instance Show Kind where
  show Pawn   = "P"
  show Knight = "N"
  show Bishop = "B"
  show Rook   = "R"
  show Queen  = "Q"
  show King   = "K"

data Color = Black | White
  deriving (Eq, Ord, Enum)

instance Show Color where
  show Black = "b"
  show White = "w"

data Piece = Piece {color::Color, kind::Kind}
  deriving (Eq, Ord)

instance Show Piece where
  show (Piece c k) = ((show c) ++ (show k))

newtype Board = Board [(Piece,Position)]

instance Show Board where
  show (Board ps) = let ordered = (sort . swap) ps
			ranks   = map (showRank ordered) [8,7..1]
			board   = intersperse "--+--+--+--+--+--+--+--" ranks
			rlabels = intersperse "  " (map (\n->(show n)++" ") [8,7..1])
			flabels = "  a  b  c  d  e  f  g  h"
		    in unlines $ zipWith (++) rlabels board ++ [flabels]
    where swap = map (\(a,b)->(b,a))
          showRank ps  r = let rnk = filter (\(p,_)->(rank p)==r) ps
	                       cs  = map (showPiece rnk) [A .. H]
			   in concat (intersperse "|" cs)
          showPiece ps f = maybe "  " (show . snd) (find (\(p,_)->(file p)==f) ps)

data Diagonal = Ascending Position | Descending Position
  deriving (Eq, Show)

-- define the diagonal according to its interesction with rank 1 or 8
-- or with file a
normalize :: Diagonal -> Diagonal
normalize d@(Ascending (Pos _ 1))  = d
normalize d@(Ascending (Pos A _))  = d
normalize (Ascending (Pos f r))    = normalize (Ascending (Pos (pred f) (r-1)))
normalize d@(Descending (Pos _ 8)) = d
normalize d@(Descending (Pos A _)) = d
normalize (Descending (Pos f r))   = normalize (Descending (Pos (pred f) (r+1)))

-- get the diagonals corresponding to a location on the board
getDiags :: Position -> (Diagonal,Diagonal)
getDiags p = (normalize (Ascending p), normalize (Descending p))

-- this is the type of our problem description
data NQueensProblem = NQP {board::Board,
                           ranks::[Rank],   files::[File],
                           asc::[Diagonal], desc::[Diagonal]}

-- initial state = empty board, all ranks, files, and diagonals free
initialState = let fileA = map (\r->Pos A r) [1..8]
                   rank8 = map (\f->Pos f 8) [A .. H]
                   rank1 = map (\f->Pos f 1) [A .. H]
                   asc   = map Ascending (nub (fileA ++ rank1))
                   desc  = map Descending (nub (fileA ++ rank8))
               in NQP (Board []) [1..8] [A .. H] asc desc

-- this is our combined monad type for this problem
type NDS a = WriterT [String] (StateT NQueensProblem []) a

-- Get the first solution to the problem, by evaluating the solver computation with
-- an initial problem state and then returning the first solution in the result list,
-- or Nothing if there was no solution.
getSolution :: NDS a -> NQueensProblem -> Maybe (a,[String])
getSolution c i = listToMaybe (evalStateT (runWriterT c) i)

-- add a Queen to the board in a specific position
addQueen :: Position -> NDS ()
addQueen p = do (Board b) <- gets board
                rs <- gets ranks
                fs <- gets files
                as <- gets asc
                ds <- gets desc
                let b'  = (Piece Black Queen, p):b
                    rs' = delete (rank p) rs
                    fs' = delete (file p) fs
                    (a,d) = getDiags p
                    as' = delete a as
                    ds' = delete d ds
                tell ["Added Queen at " ++ (show p)]
                put (NQP (Board b') rs' fs' as' ds')

-- test if a position is in the set of allowed diagonals
inDiags :: Position -> NDS Bool
inDiags p = do let (a,d) = getDiags p
               as <- gets asc
               ds <- gets desc
               return $ (elem a as) && (elem d ds)
	       
-- add a Queen to the board in all allowed positions
addQueens :: NDS ()
addQueens = do rs <- gets ranks
               fs <- gets files
               allowed <- filterM inDiags [Pos f r | f <- fs, r <- rs]
               tell [show (length allowed) ++ " possible choices"]
               msum (map addQueen allowed)

-- Start with an empty chess board and add the requested number of queens,
-- then get the board and print the solution along with the log
main :: IO ()
main = do args <- getArgs
          let n    = read (args!!0)
              cmds = replicate n addQueens
              sol  = (`getSolution` initialState) $ do sequence_ cmds
                                                       gets board
          case sol of
            Just (b,l) -> do putStr $ show b    -- show the solution
                             putStr $ unlines l -- show the log
            Nothing    -> putStrLn "No solution"

-- END OF FILE
