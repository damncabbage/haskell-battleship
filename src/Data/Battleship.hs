{-# LANGUAGE ViewPatterns #-}

-- TODO: Fold for placements on a board
-- TODO: Lazy DAG of ship placement possibilities
--         order placements from big ship to little
--         mapM placeShip [placement]?
-- TODO:
-- TODO: (Coord (x,y),Direction) -> (TL(x,y), BR(x,y))
-- TODO: Positive a / getPositive for generation of coords

module Data.Battleship (
  Direction(..), Result, Player, Coords, Ship, ShipPlacement, Shot, Board, Game, Dimensions,
  placements, -- TODO: Exporting record fields is unsafe.
  defaultShips, shipsFromList,
  mkEmptyBoard, mkRandomBoard, -- mkCoords,
  placeShip, attack, finished, winner,
  boardFromList, boardLargeEnoughForShips
) where

import Data.Maybe
import Data.Monoid
import Data.List (maximum)
import Control.Monad
import System.Random

-- TODO: Custom Shows
data Direction = Downward | Rightward  deriving(Show,Eq)
data Result      = Hit Ship | Miss       deriving(Show)
data Player      = Player1 | Player2     deriving(Show)
-- data Coords      = Coords Int Int        deriving(Show)
data Ship        = Ship {
                     name           :: String,
                     shipDimensions :: Dimensions
                   } deriving(Show,Eq)

type Dimensions      = (Int,Int)
type Coords          = (Int,Int)
type ShipPlacement   = (Ship,Coords,Direction)
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

-- mkCoords :: (Int,Int) -> Board -> Maybe Coords
-- mkCoords coords@(cx,cy) (boardDimensions -> (bx,by))
--     | valid cx bx && valid cy by = Just (Coords cx cy)
--     | otherwise                  = Nothing
--   where
--     valid c b = c > 0 && c <= b

-- (Ship,Coords,Direction)
placeShip :: Board -> ShipPlacement -> Maybe Board
placeShip b p
    | validPlacement b p = Just b { placements = placements b <> [p] }
    | otherwise          = Nothing
  where
    validPlacement b p =
      inBounds (boardDimensions b) p && not (any (overlapping p) (placements b))
    inBounds b pnew@(ship,coords,dir) =
      True -- error "TODO"
    overlapping p1 p2 =
      let x1 = fst . topLeft
          x2 = fst . bottomRight
          y1 = snd . topLeft
          y2 = snd . bottomRight
      in (x1 p1 < x2 p2) && (x2 p1 > x1 p2) &&
         (y1 p1 < y2 p2) && (y2 p1 > y1 p2)
    topLeft (_,p,_) = p
    bottomRight (shipDimensions -> (dx,dy), (cx,cy), dir) =
      case dir of
        Downward  -> (cx + dx, cy + dy)
        Rightward -> (cx + dy, cy + dx)

boardFromList :: Board -> [ShipPlacement] -> Maybe Board
boardFromList = foldM placeShip

attack :: Game -> Player -> Shot -> Game
attack = error "TODO"

finished :: Game -> Bool
finished = error "TODO"

winner :: Game -> Maybe Player
winner = error "TODO"
