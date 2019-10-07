module TicTacToe where

import Data.List (intercalate, intersperse, find)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

data Mark = X | O | None deriving (Eq, Show)

data Board = Board { grid :: [[Mark]], next :: Mark } deriving Show

defaultBoard :: Board
defaultBoard = Board { grid = [[None, None, None],
                               [None, None, None],
                               [None, None, None]],
                       next = None }

prettyBoard :: Board -> String
prettyBoard board = intercalate "\n-+-+-\n" $ map makeLine $ grid board
                      where makeLine line = intersperse '|' $ map showMark line
                            showMark mark = case mark of
                                              X -> 'X'
                                              O -> 'O'
                                              None -> ' '

replace :: Int -> a -> [a] -> [a]
replace index item list = left ++ (item:right)
                            where (left, (_:right)) = splitAt index list

setMove :: Board -> Int -> Board
setMove board pos = board { grid = replace rowIndex newRow (grid board) }
                      where newRow = replace colIndex (next board) oldRow
                            oldRow = (grid board) !! rowIndex
                            rowIndex = (pos - 1) `quot` 3
                            colIndex = (pos - 1) `mod` 3
                      
boardWon :: Board -> Mark
boardWon board = fromMaybe None $ find (/= None) $ map checkLine lines'
                   where checkLine line = let marks = map (\(r, c) -> grid board !! r !! c) line
                                          in if all (== marks !! 0) marks
                                             then marks !! 0
                                             else None
                         lines' = [[(0,0), (0,1), (0,2)],
                                   [(1,0), (1,1), (1,2)],
                                   [(2,0), (2,1), (2,2)],
                                   [(0,0), (1,0), (2,0)],
                                   [(0,1), (1,1), (2,1)],
                                   [(0,2), (1,2), (2,2)],
                                   [(0,0), (1,1), (2,2)],
                                   [(0,2), (1,1), (2,0)]]
                                          
inputMoveNum :: Board -> IO Int
inputMoveNum board = do
  putStr "Input move (1-9): "
  hFlush stdout
  pos <- getLine
  case readMaybe pos :: Maybe Int of
    Just x -> if x >= 1 && x <= 9
                then let rowIndex = (x - 1) `quot` 3
                         colIndex = (x - 1) `mod` 3
                     in if grid board !! rowIndex !! colIndex == None 
                        then return x
                        else do
                          putStrLn "Invalid move, try again"
                          inputMoveNum board
                else do
                  putStrLn "Number out of range, try again"
                  inputMoveNum board
    Nothing -> do
      putStrLn "Invalid input, try again"
      inputMoveNum board

inputMove :: Board -> IO Int
inputMove board = do
  putStrLn $ prettyBoard board
  putStrLn $ "Next player: " ++ (show $ next board)
  inputMoveNum board

nextPlayer :: Mark -> Mark
nextPlayer mark = if mark == X
                  then O
                  else X

runGame :: Board -> IO Mark
runGame board = let winner' = boardWon board
                in if winner' /= None
                   then do
                     putStrLn $ prettyBoard board
                     putStrLn $ "Congratulations " ++ show winner' ++ "! You won"
                     return winner'
                   else do
                     move <- inputMove board
                     runGame $ (setMove board move) { next = nextPlayer $ next board }

main :: IO Mark
main = runGame defaultBoard { next = X }