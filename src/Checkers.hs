module Checkers
    ( 
    ) where

import qualified Data.Map as Map

data Position = Position Int Int deriving (Eq, Ord)

data Color = Black | White
data Piece = Men Color | King Color

newtype Board = Board (Map.Map Position Piece)

startBoard :: Board
startBoard = Board (Map.fromList (
    map (makeTuple White . makePos 0) left ++
    map (makeTuple White . makePos 1) right ++
    map (makeTuple White . makePos 2) left ++
    map (makeTuple White . makePos 3) right ++
    map (makeTuple Black . makePos 6) left ++
    map (makeTuple Black . makePos 7) right ++
    map (makeTuple Black . makePos 8) left ++
    map (makeTuple Black . makePos 9) right
    ))
    where
        left                = [0, 2, 4, 6, 8]
        right               = [1, 3, 5, 7, 9]
        makePos row col     = Position col row
        makeTuple color pos = (pos, Men color)