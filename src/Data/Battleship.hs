{-# LANGUAGE ViewPatterns #-}

module Data.Battleship (
  Orientation, Result, Ship, Player, Coords, ShipPlacement, Shot, Board, Game, BoardDimensions,
  defaultShips, shipsFromList,
  mkEmptyBoard, mkRandomBoard, mkCoords,
  placeShip, attack, finished, winner
) where

import Data.Maybe
import Data.Monoid
import System.Random

-- TODO: Custom Shows
data Orientation = Down | Right          deriving(Show)
data Result      = Hit Ship | Miss       deriving(Show)
data Ship        = Ship String (Int,Int) deriving(Show)
data Player      = Player1 | Player2     deriving(Show)
data Coords      = Coords Int Int        deriving(Show)

type BoardDimensions = (Int,Int)
type ShipPlacement   = (Ship,Coords,Orientation)
type Shot            = (Player,Coords)

data Board = Board {
               size       :: BoardDimensions,
               placements :: [ShipPlacement],
               shots      :: [(Shot,Result)]
             } deriving(Show)
data Game  = Game {
               player1    :: (Player,Board),
               player2    :: (Player,Board),
               validShips :: [Ship]
             } deriving(Show)

defaultShips :: [Ship]
defaultShips = shipsFromList [
                 ("Carrier",    (1,5)),
                 ("Battleship", (1,4)),
                 ("Submarine",  (1,3)),
                 ("Cruiser",    (1,2)),
                 ("Patrol",     (1,1))
               ]

-- TODO: Another Maybe? Not constraining creation of ships.
shipsFromList :: [(String,(Int,Int))] -> [Ship]
shipsFromList = map (\(n,p) -> Ship n p)

-- Headlining operations
-- First stab: branching, rather than encoding board states in types.
-- TODO: This is all assuming a single board; need to encode the board pair with ownerships.
mkEmptyBoard :: BoardDimensions -> Maybe Board
mkEmptyBoard size@(x,y)
  | x > 0 && y > 0 = Just Board { size = size, placements = [], shots = [] }
  | otherwise      = Nothing

mkRandomBoard :: BoardDimensions -> [Ship] -> (IO StdGen) -> Maybe Board
mkRandomBoard size ships gen = do
  board <- mkEmptyBoard size
  error "TODO"

mkCoords :: (Int,Int) -> Board -> Maybe Coords
mkCoords coords@(cx,cy) (size -> (bx,by))
    | valid cx bx && valid cy by = Just (Coords cx cy)
    | otherwise                  = Nothing
  where
    valid c b = c > 0 && c <= b

-- (Ship,Coords,Orientation)
placeShip :: Board -> ShipPlacement -> Maybe Board
placeShip b p
    | validPlacement b p = Just b { placements = placements b <> [p] } -- TODO: Replace with Lens in three years.
    | otherwise          = Nothing
  where
    validPlacement b p = noOverlaps (placements b) p && inBounds (size b) p
    noOverlaps ps pnew@(ship,coords,orient) = error "TODO"
    inBounds b pnew@(ship,coords,orient)    = error "TODO"


attack :: Game -> Player -> Shot -> Game
attack = error "TODO"

finished :: Game -> Bool
finished = error "TODO"

winner :: Game -> Maybe Player
winner = error "TODO"
