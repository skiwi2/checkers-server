module Checkers
    (
    ) where

import qualified Data.Map as Map

data Position = Position Int Int deriving (Eq, Ord)

data Color = Black | White deriving (Eq)
data Piece = Men Color | King Color

newtype Board = Board (Map.Map Position Piece)

data Move = Move Position Color Position

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

isValidPosition :: Position -> Board -> Bool
isValidPosition pos@(Position x y) (Board m) = x >= 0 && x <= 9 && y >= 0 && y <= 9 && Map.notMember pos m

isValidMove :: Move -> Board -> Bool
isValidMove (Move _ _ to) = isValidPosition to

getMoves :: Board -> [Move]
getMoves board@(Board m) = filter (`isValidMove` board) $ concatMap getMoves' $ Map.toList m

getMoves' :: (Position, Piece) -> [Move]
getMoves' (pos@(Position x y), piece) = case piece of
    Men White   -> map (Move pos White) [Position (y + 1) (x - 1), Position (y + 1) (x + 1)]
    Men Black   -> map (Move pos Black) [Position (y - 1) (x - 1), Position (y - 1) (x - 1)]
    King White  -> error "TODO: getMoves' King White"
    King Black  -> error "TODO: getMoves' King Black"

getMovesByColor :: Board -> Color -> [Move]
getMovesByColor board = getMovesByColor' $ getMoves board

getMovesByColor' :: [Move] -> Color -> [Move]
getMovesByColor' moves color = filter (\(Move _ color' _) -> color == color') moves