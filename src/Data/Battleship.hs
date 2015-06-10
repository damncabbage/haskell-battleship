module Data.Battleship (
  Orientation, Result, Ship, Player, Coordinates, ShipPlacement, Shot, Board, Game,
  mkEmptyBoard, mkRandomBoard, placeShip, attack, finished, winner
) where

import Data.Maybe
import System.Random

-- TODO: Custom Shows
data Orientation = Down | Right                      deriving(Show)
data Result      = Hit Ship | Miss                   deriving(Show)
data Ship        = Ship String (Int,Int)             deriving(Show)
data Player      = Player1 | Player2                 deriving(Show)

-- TODO: Coords can't be a type alias; we need to have a restricted constructor, otherwise
--       we'll allow (-1,900000) or something.
type Coordinates   = (Int,Int)
type BoardSize     = (Int,Int)
type ShipPlacement = (Ship,Coordinates,Orientation)
type Shot          = (Player,Coordinates)

data Board = Board {
               size       :: BoardSize,
               placements :: [ShipPlacement],
               shots      :: [(Shot,Result)]
             } deriving(Show)
data Game  = Game {
               player1    :: (Player,Board),
               player2    :: (Player,Board),
               validShips :: [Ship]
             } deriving(Show)

defaultShips :: [Ship]
defaultShips = map (\(n,p) -> Ship n p) [
                 ("Carrier",    (1,5)),
                 ("Battleship", (1,4)),
                 ("Submarine",  (1,3)),
                 ("Cruiser",    (1,2)),
                 ("Patrol",     (1,1))
               ]

-- Headlining operations
-- First stab: branching, rather than encoding board states in types.
-- TODO: This is all assuming a single board; need to encode the board pair with ownerships.
mkEmptyBoard :: BoardSize -> Maybe Board
mkEmptyBoard size =
  Just Board { size = size, placements = [], shots = [] }

mkRandomBoard :: BoardSize -> StdGen -> Maybe Board
mkRandomBoard size gen = do
  board <- mkEmptyBoard size
  error "TODO"

mkCoordinates :: (Int,Int) -> BoardSize -> Maybe Coordinates
mkCoordinates (cx,cy) (bw,bh) = error "TODO"

placeShip :: Board -> ShipPlacement -> Board
placeShip = error "TODO"

attack :: Game -> Player -> Shot -> Game
attack = error "TODO"

finished :: Game -> Bool
finished = error "TODO"

winner :: Game -> Maybe Player
winner = error "TODO"
