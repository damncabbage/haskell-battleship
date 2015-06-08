module Data.Battleship where

import Data.Maybe
import System.Random

-- TODO: Custom Shows
data Orientation = Down | Right                      deriving(Show)
data Result      = Hit Ship | Miss                   deriving(Show)
data Ship        = Ship String (Int,Int)             deriving(Show)
data Player      = Player1 | Player2                 deriving(Show)

type Coordinates = (Int,Int)
type Placement   = (Ship,Coordinates,Orientation)
type Shot        = (Player,Coordinates)

data Board       = Board [Placement] [(Shot,Result)] deriving(Show)

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
mkEmptyBoard :: Board
mkEmptyBoard = Board [] []

mkRandomBoard :: IO StdGen -> IO Board
mkRandomBoard = error "TODO"

placeShip :: Board -> Placement -> Board
placeShip = error "TODO"

attack :: Board -> Shot -> Board
attack = error "TODO"

finished :: Board -> Bool
finished = error "TODO"

winner :: Board -> Maybe Player
winner = error "TODO"
