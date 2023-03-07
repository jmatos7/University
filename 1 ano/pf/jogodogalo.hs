import Data.List (transpose)

data Game = Game {
  board :: [[Char]],
  turn :: Char
}

-- Initialize the game board
initGame :: Game
initGame = Game { board = replicate 3 (replicate 3 ' '), turn = 'X' }

-- Print the current game board
printBoard :: Game -> IO ()
printBoard (Game board _) = putStr (unlines board)

-- Check if the game is over
gameOver :: Game -> Bool
gameOver (Game board _) = any (all (== 'X')) board || any (all (== 'O')) board || all (all (/= ' ')) board

-- Check if the current player has won
winner :: Game -> Char
winner (Game board turn) = if any (all (== turn)) board || any (all (== turn)) (transpose board) then turn
                           else if (all (== turn) [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2]) || (all (== turn) [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]) then turn
                           else ' '

-- Get the player's move
getMove :: Game -> IO (Int, Int)
getMove (Game _ turn) = do
  putStrLn (turn : "'s turn")
  putStr "Enter row: "
  row <- readLn :: IO Int
  putStr "Enter column: "
  col <- readLn :: IO Int
  return (row, col)

-- Update the game board with the player's move
updateBoard :: Game -> (Int, Int) -> Game
updateBoard (Game board turn) (row, col) = Game { board = newBoard, turn = if turn == 'X' then 'O' else 'X' }
  where newBoard = take row board ++ [take col (board !! row) ++ turn : drop (col + 1) (board !! row)] ++ drop (row + 1) board

-- Main game loop
playGame :: Game -> IO ()
playGame game = do
  printBoard game
  if gameOver game then do
    putStrLn "Game over!"
    putStrLn ("Winner: " ++ [winner game])
  else do
    move <- getMove game
    playGame (updateBoard game move)

main :: IO ()
main = playGame initGame
