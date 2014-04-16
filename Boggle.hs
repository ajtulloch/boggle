{-# LANGUAGE RecordWildCards #-}
-- |Parallel Boggle Solver
module Main where

import           Control.Monad.Random        hiding (fromList)
import           Control.Parallel.Strategies
import qualified Data.HashSet                as HS
import           Data.Packed.Matrix          hiding (cols, rows)
import           Data.Packed.Vector
import qualified Data.Text                   as T
import           System.Environment

-- * Boggle Solver
-- ** Data Structures
-- |`Dictionary` enscapsulates an efficient implementation of finding
-- whether a given word is in the dictionary
data Dictionary = Dictionary { isWord :: T.Text -> Bool }

-- |`Position` is a (row, column) point on a board
type Position = (Int, Int)

-- |`Board` encapsulates a given Boggle board.
data Board = Board {
      rows   :: Int,
      cols   :: Int,
      values :: Position -> Char }

-- |`BoggleState`` contains all information about a path in our DFS
-- traversal of the board.
data BoggleState = BoggleState {
      accumulated :: T.Text,
      visited     :: HS.HashSet Position,
      current     :: Position }

-- ** Solving routines
-- |`neighbours` compute all valid neighbours of a given state on our
-- Boggle board, obeying all rules regarding visiting previous
-- positions, valid board positions, and so on.
neighbours :: Board -> BoggleState -> [BoggleState]
neighbours b@Board{..} s@BoggleState{..} =
    (map (takeStep b s) . filter (isValid b s)) (candidates current)

takeStep :: Board -> BoggleState -> Position -> BoggleState
takeStep Board{..} BoggleState{..} pos =  BoggleState {
                                            accumulated=acc,
                                            visited=HS.insert pos visited,
                                            current=pos }
    where
      acc = T.append accumulated (T.singleton $ values pos)

isValid :: Board -> BoggleState -> Position -> Bool
isValid Board{..} BoggleState{..} pos@(candidateRow, candidateColumn) =
          and [-- not visited this position yet
               not (HS.member pos visited),
               -- row, column within bounds
               0 <= candidateRow, candidateRow < rows,
               0 <= candidateColumn, candidateColumn < cols]

candidates :: Position -> [Position]
candidates (row, column) = [(row + rdelta, column + cdelta) |
                            rdelta <- [-1, 0, 1],
                            cdelta <- [-1, 0, 1]]

-- |`boggle` is the main entrance point for our solver.
boggle :: Dictionary -> Board -> HS.HashSet T.Text
boggle Dictionary{..} board@(Board{..}) = runFrom startingPositions
    where
      runFrom = HS.fromList . parallelExpand . map initialState
      -- construct candidate words in parallel
      parallelExpand = concat . parMap rdeepseq expand
      -- explores and combines all children solutions from the current
      -- state
      expand current@(BoggleState accumulated _ _) =
          let descendents = parallelExpand (neighbours board current) in
          if isWord accumulated then accumulated:descendents else descendents
      -- starting positions with zero-based indexing
      startingPositions = [(i-1, j-1) | i <- [1..rows], j <- [1..cols]]
      initialState pos = BoggleState {
                           accumulated=T.singleton $ values pos,
                           visited=HS.singleton pos,
                           current=pos }


-- * Dictionary and Board construction
-- |Read a given filepath into a dictionary, with one word per line.
readDict :: FilePath -> IO Dictionary
readDict path = do
  fileContents <- readFile path
  let processed = (HS.fromList .
                   filter (\s -> T.length s >= 3) .
                   map (T.strip . T.pack) . lines) fileContents
  return $ Dictionary (`HS.member` processed)

-- |`randomBoard` constructs a random boggle board of the given size.
randomBoard :: (MonadRandom m) => Int -> m Board
randomBoard n = do
  chars <- choices "abcdefghijklmnopqrstuvwxyz"
  let board = (reshape n . fromList . take (n * n)) chars
  return $ Board n n (board @@>)
      where
        choices candidates = do
                  samples <- getRandomRs (0, length candidates - 1)
                  return $ map (candidates !!) samples

-- * Main entry points
-- |Solves the boggle instance with a given board size and using a
-- dictionary from `/usr/share/dict/words`
runBoggle :: Int -> IO (HS.HashSet T.Text)
runBoggle n = do
  dict <- readDict "/usr/share/dict/words"
  board <- evalRandIO $ randomBoard n
  return $ boggle dict board

-- |Reads instance size from command line and solves a random Boggle
-- instance.
main :: IO ()
main = do
  [n] <- getArgs
  solutions <- runBoggle (read n)
  print solutions
