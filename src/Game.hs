module Game 
    (
    ) where

import Board

data Game = Game Board Color deriving (Eq, Show)

startGame :: Color -> Game
startGame = Game startBoard

getPlayerMoves :: Color -> Game -> [Move]
getPlayerMoves color (Game board _) = filter (isMoveBy color) (getMoves board)

processMove :: Move -> Game -> Game
processMove move (Game board color) = do
    let board' = applyMove move board
    if hasCaptureMoves color board'
        then Game board' color
        else Game board' (getOpponent color)

getOpponent :: Color -> Color
getOpponent White = Black
getOpponent Black = White