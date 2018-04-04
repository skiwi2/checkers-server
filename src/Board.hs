module Board
    ( Board
    , Position
    , Piece
    , PieceKind
    , Player(..)
    , Action
    , Move
    , startBoard
    , getMoves
    , isMoveBy
    , hasCaptureMoves
    , applyMove
    ) where

import qualified Data.Map as Map

data Position = Position Int Int deriving (Eq, Ord, Show)

data Player = Black | White deriving (Eq, Show)
data PieceKind = Men | King deriving (Eq, Show)
data Piece = Piece PieceKind Player deriving (Eq, Show)

newtype Board = Board (Map.Map Position Piece) deriving (Eq, Show)

data Action = PieceMove | PieceCapture Position deriving (Eq, Show)
-- Move (from, by, how, becomes king, to)
data Move = Move Position Player Action Bool Position deriving (Eq, Show)

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
        left                    = [0, 2, 4, 6, 8]
        right                   = [1, 3, 5, 7, 9]
        makePos row col         = Position col row
        makeTuple player pos    = (pos, Piece Men player)

isValidPosition :: Position -> Bool
isValidPosition (Position x y) = x >= 0 && x <= 9 && y >= 0 && y <= 9

-- TODO for kings that capture there may only be one piece in between the to and from position
isValidMove :: Move -> Board -> Bool
isValidMove (Move _ player action _ to) (Board m) = isValidPosition to && Map.notMember to m && validAction action
    where
        validAction act     = case act of
            PieceMove           -> True
            PieceCapture pos    -> opponentOnMap pos 
        opponentOnMap pos   = case Map.lookup pos m of
            Nothing                 -> False
            Just (Piece _ player')  -> player' /= player

applyMove :: Move -> Board -> Board
applyMove (Move from _ action becomesKing to) (Board m) = case action of
    PieceMove               -> Board $ movePiece
    PieceCapture capturePos -> Board $ Map.delete capturePos m
    where
        movePiece = do
            let piece = m Map.! from
            Map.insert to (promotePiece piece) $ Map.delete from m
        promotePiece (Piece Men player) = if becomesKing
            then Piece King player
            else Piece Men player
        promotePiece p@(Piece King _) = p

getMoves :: Board -> [Move]
getMoves board@(Board m) = do
    let moves = filter (`isValidMove` board) $ concatMap getMoves' $ Map.toList m
    if any isCapturingMove moves
        then filter isCapturingMove moves
        else moves

getMoves' :: (Position, Piece) -> [Move]
getMoves' (pos@(Position x y), piece) = case piece of
    Piece Men White     -> map (makeMove pos White PieceMove) [Position (x + 1) (y - 1), Position (x + 1) (y + 1)] ++ captureMoves pos White
    Piece Men Black     -> map (makeMove pos Black PieceMove) [Position (x - 1) (y - 1), Position (x - 1) (y - 1)] ++ captureMoves pos Black
    Piece King White    -> error "TODO: getMoves' King White"
    Piece King Black    -> error "TODO: getMoves' King Black"
    where
        captureMoves from player = [
            makeMove from player (PieceCapture (Position (x + 1) (y - 1))) (Position (x + 2) (y - 2)),
            makeMove from player (PieceCapture (Position (x + 1) (y + 1))) (Position (x + 2) (y + 2)),
            makeMove from player (PieceCapture (Position (x - 1) (y - 1))) (Position (x + 2) (y - 2)),
            makeMove from player (PieceCapture (Position (x - 1) (y + 1))) (Position (x - 2) (y + 2))]

hasCaptureMoves :: Player -> Board -> Bool
hasCaptureMoves player board = any isCapturingMove $ filter (isMoveBy player) $ getMoves board

makeMove :: Position -> Player -> Action -> Position -> Move
makeMove from player action to@(Position _ y) = Move from player action becomesKing to
    where
        becomesKing = case player of
            White   -> y == 9
            Black   -> y == 0

isMoveBy :: Player -> Move -> Bool
isMoveBy player (Move _ player' _ _ _) = player == player'

isCapturingMove :: Move -> Bool
isCapturingMove (Move _ _ PieceMove _ _) = False
isCapturingMove (Move _ _ (PieceCapture _) _ _) = True