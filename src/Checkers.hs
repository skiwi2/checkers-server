module Checkers
    (
    ) where

import qualified Data.Map as Map

data Position = Position Int Int deriving (Eq, Ord)

data Color = Black | White deriving (Eq)
data PieceKind = Men | King
data Piece = Piece PieceKind Color

newtype Board = Board (Map.Map Position Piece)

data Action = PieceMove | PieceCapture Position
-- Move (from, by, how, becomes king, to)
data Move = Move Position Color Action Bool Position

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
        makeTuple color pos = (pos, Piece Men color)

isValidPosition :: Position -> Bool
isValidPosition (Position x y) = x >= 0 && x <= 9 && y >= 0 && y <= 9

isValidMove :: Move -> Board -> Bool
isValidMove (Move _ color action _ to) (Board m) = isValidPosition to && Map.notMember to m && validAction action
    where
        validAction act     = case act of
            PieceMove           -> True
            PieceCapture pos    -> opponentOnMap pos 
        opponentOnMap pos   = case Map.lookup pos m of
            Nothing                 -> False
            Just (Piece _ pColor)   -> pColor == opposingColor color

opposingColor :: Color -> Color
opposingColor color = case color of
    White   -> Black
    Black   -> White

getMoves :: Board -> [Move]
getMoves board@(Board m) = do
    let moves = filter (`isValidMove` board) $ concatMap getMoves' $ Map.toList m
    if any isCapturingMove moves
        then filter isCapturingMove moves
        else moves
    where
        isCapturingMove (Move _ _ action _ _) = case action of
            PieceMove       -> False
            PieceCapture _  -> True

getMoves' :: (Position, Piece) -> [Move]
getMoves' (pos@(Position x y), piece) = case piece of
    Piece Men White     -> map (makeMove pos White PieceMove) [Position (x + 1) (y - 1), Position (x + 1) (y + 1)] ++ captureMoves pos White
    Piece Men Black     -> map (makeMove pos Black PieceMove) [Position (x - 1) (y - 1), Position (x - 1) (y - 1)] ++ captureMoves pos Black
    Piece King White    -> error "TODO: getMoves' King White"
    Piece King Black    -> error "TODO: getMoves' King Black"
    where
        captureMoves from color = [
            makeMove from color (PieceCapture (Position (x + 1) (y - 1))) (Position (x + 2) (y - 2)),
            makeMove from color (PieceCapture (Position (x + 1) (y + 1))) (Position (x + 2) (y + 2)),
            makeMove from color (PieceCapture (Position (x - 1) (y - 1))) (Position (x + 2) (y - 2)),
            makeMove from color (PieceCapture (Position (x - 1) (y + 1))) (Position (x - 2) (y + 2))]

makeMove :: Position -> Color -> Action -> Position -> Move
makeMove from color action to@(Position _ y) = Move from color action becomesKing to
    where
        becomesKing = case color of
            White   -> y == 9
            Black   -> y == 0

getMovesByColor :: Board -> Color -> [Move]
getMovesByColor board = getMovesByColor' $ getMoves board

getMovesByColor' :: [Move] -> Color -> [Move]
getMovesByColor' moves color = filter (\(Move _ color' _ _ _) -> color == color') moves