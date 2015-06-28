{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

-- TODO: Fold for placements on a board
-- TODO: Lazy DAG of ship placement possibilities
--         order placements from big ship to little
--         mapM placeShip [placement]?
-- TODO:
-- TODO: (Coord (x,y),Direction) -> (TL(x,y), BR(x,y))
-- TODO: Positive a / getPositive for generation of coords

module Data.Battleship (
  -- Types and (safe) constructors
  Board,
  Coords,
  Dimensions,
  Direction(..),
  Game,
  Player(..),
  Result,
  Ship,
  ShipPlacement,
  Shot,

  -- Headlining Library Functions
  mkEmptyBoard,
  mkRandomBoard,
  mkGame,
  placeShip,
  attack,
  finished,
  winner,

  -- Helpers (for tests, but still safe)
  defaultShips,
  defaultDimensions,
  shipsFromList,
  boardLargeEnoughForShips,
  placedBoardFromList,
  attacksFromList,

  -- HACK TODO: Exporting record fields is /not/ safe.
  board1,
  board2,
  placements,
  shots
) where

import Data.Maybe
import Data.Monoid
import Data.List (maximum)
import Control.Monad
import System.Random
import Text.Printf

data Direction = Downward | Rightward deriving(Show,Eq)
data Result    = Hit Ship | Miss      deriving(Show,Eq)
data Player    = Player1 | Player2    deriving(Show,Eq)
data Ship      = Ship {
                   name           :: String,
                   initial        :: Char,
                   shipDimensions :: Dimensions
                 } deriving(Show,Eq)

type Dimensions      = (Int,Int)
type Coords          = (Int,Int)
type ShipPlacement   = (Ship,Coords,Direction)
type Shot            = (Player,Coords)

data Board = Board {
               boardDimensions :: Dimensions,
               placements      :: [ShipPlacement],
               shots           :: [(Player,Coords,Result)],
               validShips      :: [Ship]
             }
data Game  = Game {
               player1 :: Player,
               board1  :: Board,
               player2 :: Player,
               board2  :: Board
             } deriving(Show)

instance Show Board where
  show b =
    unlines $ [header] <> (map row [1..h])
    where
      w          = (fst . boardDimensions) b
      h          = (snd . boardDimensions) b
      itemWidth  = 1 + (length $ show h)
      item       = printf ("%-" <> (show itemWidth) <> "s")
      tlPadding  = replicate itemWidth ' '
      header     = tlPadding <> (concatMap (item . show) [1..w])
      row n      = concat [item (show n), concatMap (cell n) [1..w]]
      cell y x   = (\c -> item [c]) $ ['A','B','X','·','·','·'] !! (((x `mod` 3) + (y `mod` 9)) `mod` 6)
      -- TODO: Look for Shots, then Ships, otherwise default to the dot.


-- TODO: One of the few cases I'd consider using a fromJust.
defaultShips :: [Ship]
defaultShips = fromMaybe [] $ shipsFromList
                 [ ("Carrier",    'C', (1,5))
                 , ("Battleship", 'B', (1,4))
                 , ("Submarine",  'S', (1,3))
                 , ("Cruiser",    'R', (1,2))
                 , ("Patrol",     'P', (1,1))
                 ]

defaultDimensions :: Dimensions
defaultDimensions = (10,10)

shipsFromList :: [(String,Char,Coords)] -> Maybe [Ship]
shipsFromList = sequence . map (\(n,i,p) -> mkShip n i p)

mkShip :: String -> Char -> (Int,Int) -> Maybe Ship
mkShip n i d
  | vDims d && vInitial i && vName n = Just Ship { name = n, initial = i, shipDimensions = d }
  | otherwise                        = Nothing
  where
    vDims (x,y) = x > 0 && y > 0
    vInitial i  = elem i (['A'..'W'] <> ['Y'..'Z']) -- All except X
    vName n     = length n > 0

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

mkGame :: (Player,Board) -> (Player,Board) -> Maybe Game
mkGame (p1,b1) (p2,b2)
  | p1 /= p2  = Just Game { player1 = p1, board1 = b1, player2 = p2, board2 = b2 }
  | otherwise = Nothing

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
    inBounds (bw,bh) (shipDimensions -> (sx,sy), (cx,cy), dir) =
      case dir of
        Downward  -> cx > 0 && cy > 0 && cx + (sx - 1) <= bw && cy + (sy - 1) <= bh
        Rightward -> cx > 0 && cy > 0 && cx + (sy - 1) <= bw && cy + (sx - 1) <= bh
    overlapping p1 p2 =
      let x1 = fst . topLeft
          x2 = fst . bottomRight
          y1 = snd . topLeft
          y2 = snd . bottomRight
      in (x1 p1 < x2 p2) && (x2 p1 > x1 p2) &&
         (y1 p1 < y2 p2) && (y2 p1 > y1 p2)
    topLeft (_,p,_) = p
    bottomRight (shipDimensions -> (sx,sy), (cx,cy), dir) =
      case dir of
        Downward  -> (cx + sx, cy + sy)
        Rightward -> (cx + sy, cy + sx)

placedBoardFromList :: Board -> [ShipPlacement] -> Maybe Board
placedBoardFromList = foldM placeShip

attack :: Game -> Shot -> Maybe Game
attack = error "TODO"

attacksFromList :: Game -> [Shot] -> Maybe Game
attacksFromList = foldM attack

finished :: Game -> Bool
finished = error "TODO"

winner :: Game -> Maybe Player
winner = error "TODO"
