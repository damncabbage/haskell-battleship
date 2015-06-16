{-# LANGUAGE ViewPatterns #-}

module Data.Battleship (
  Orientation, Result, Player, Coords, Ship, ShipPlacement, Shot, Board, Game, Dimensions,
  defaultShips, shipsFromList,
  mkEmptyBoard, mkRandomBoard, mkCoords,
  placeShip, attack, finished, winner,
  boardLargeEnoughForShips
) where

import Data.Maybe
import Data.Monoid
import Data.List (maximum)
import System.Random

-- TODO: Custom Shows
data Orientation = Down | Right          deriving(Show)
data Result      = Hit Ship | Miss       deriving(Show)
data Player      = Player1 | Player2     deriving(Show)
data Coords      = Coords Int Int        deriving(Show)
data Ship        = Ship {
                     name           :: String,
                     shipDimensions :: Dimensions
                   } deriving(Show)

type Dimensions      = (Int,Int)
type ShipPlacement   = (Ship,Coords,Orientation)
type Shot            = (Player,Coords)

data Board = Board {
               boardDimensions :: Dimensions,
               placements      :: [ShipPlacement],
               shots           :: [(Shot,Result)],
               validShips      :: [Ship]
             } deriving(Show)
data Game  = Game {
               player1    :: (Player,Board),
               player2    :: (Player,Board)
             } deriving(Show)

-- TODO: One of the few cases I'd consider using a fromJust.
defaultShips :: [Ship]
defaultShips = fromMaybe [] $ shipsFromList [
                 ("Carrier",    (1,5)),
                 ("Battleship", (1,4)),
                 ("Submarine",  (1,3)),
                 ("Cruiser",    (1,2)),
                 ("Patrol",     (1,1))
               ]

-- TODO: Another Maybe? Not constraining creation of ships.
shipsFromList :: [(String,(Int,Int))] -> Maybe [Ship]
shipsFromList = sequence . map (\(n,p) -> mkShip n p)

mkShip :: String -> (Int,Int) -> Maybe Ship
mkShip n d@(x,y)
  | x > 0 && y > 0 && length n > 0 = Just Ship { name = n, shipDimensions = d }
  | otherwise                      = Nothing

-- TODO: Appears to hold up for everything I've tossed at it. Needs a property test,
--       but it's damn hard to come up with a generator producing non-overlapping ships.
boardLargeEnoughForShips :: Dimensions -> [Ship] -> Bool
boardLargeEnoughForShips (x,y) ships =
    (largestDim >= longestShip) && (x * y >= shipsArea)
  where
    largestDim  = max x y
    longestShip = (maximum . mconcat . map (pairToList . shipDimensions)) ships
    shipsArea   = (sum . map (multList . pairToList . shipDimensions)) ships
    multList    = foldr (*) 1
    pairToList (a,b) = [a,b]

mkEmptyBoard :: Dimensions -> [Ship] -> Maybe Board
mkEmptyBoard d s =
  if not (null s) && boardLargeEnoughForShips d s then
    Just Board { boardDimensions = d, placements = [], shots = [], validShips = s }
  else
    Nothing

mkRandomBoard :: Dimensions -> [Ship] -> (IO StdGen) -> Maybe Board
mkRandomBoard dims ships gen = do
  board <- mkEmptyBoard dims ships
  error "TODO"

mkCoords :: (Int,Int) -> Board -> Maybe Coords
mkCoords coords@(cx,cy) (boardDimensions -> (bx,by))
    | valid cx bx && valid cy by = Just (Coords cx cy)
    | otherwise                  = Nothing
  where
    valid c b = c > 0 && c <= b

-- (Ship,Coords,Orientation)
placeShip :: Board -> ShipPlacement -> Maybe Board
placeShip b p
    | validPlacement b p = Just b { placements = placements b <> [p] } -- TODO: Replace with Lens in two years.
    | otherwise          = Nothing
  where
    validPlacement b p = noOverlaps (placements b) p && inBounds (boardDimensions b) p
    noOverlaps ps pnew@(ship,coords,orient) = error "TODO"
    inBounds b pnew@(ship,coords,orient)    = error "TODO"


attack :: Game -> Player -> Shot -> Game
attack = error "TODO"

finished :: Game -> Bool
finished = error "TODO"

winner :: Game -> Maybe Player
winner = error "TODO"
