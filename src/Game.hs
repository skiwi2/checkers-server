module Game 
    (
    ) where

import Board

data Game = Game Board Player deriving (Eq, Show)

startGame :: Player -> Game
startGame = Game startBoard

getPlayerMoves :: Player -> Game -> [Move]
getPlayerMoves player (Game board _) = filter (isMoveBy player) (getMoves board)

processMove :: Move -> Game -> Game
processMove move (Game board player) = do
    let board' = applyMove move board
    if hasCaptureMoves player board'
        then Game board' player
        else Game board' (getOpponent player)

getOpponent :: Player -> Player
getOpponent White = Black
getOpponent Black = White