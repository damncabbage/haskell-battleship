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
  boardFinished,

  -- HACK TODO: Exporting record fields is /not/ safe.
  board1,
  board2,
  placements,
  shots
) where

import Data.Maybe
import Data.Monoid
import Data.List (find,intersect,maximum)
import Control.Monad
import System.Random
import Text.Printf

data Direction = Downward | Rightward     deriving(Show,Eq)
data Result    = Hit ShipPlacement | Miss deriving(Show,Eq)
data Player    = Player1 | Player2        deriving(Show,Eq)
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
attack g (p,c) =
    if inBounds board c && notRepeated && isHit then
      -- HACK: Record setter case-matching repetition I'm not sure can be addressed without lenses.
      case p of
        Player1 -> Just g { board2 = appendShot }
        Player2 -> Just g { board1 = appendShot }
    else
      Nothing
  where
    inBounds (boardDimensions -> (bw,bh)) (cx,cy) = True -- TODO
    notRepeated = True -- TODO: Implement, add easy property
    ships       = placements board
    result      = maybe Miss Hit (find (elem c . shipPlacementToCoords) ships)
    isHit       = case result of (Hit _) -> True; Miss -> False
    appendShot  = board { shots = (shots board) <> [(p,c,result)] }
    board       = case p of
      Player1 -> board2 g
      Player2 -> board1 g

attacksFromList :: Game -> [Shot] -> Maybe Game
attacksFromList = foldM attack

finished :: Game -> Bool
finished g = boardFinished (board1 g) || boardFinished (board2 g)

winner :: Game -> Maybe Player
winner g
  | boardFinished (board1 g) = Just (player1 g)
  | boardFinished (board2 g) = Just (player2 g)
  | otherwise                = Nothing

boardFinished :: Board -> Bool
boardFinished b =
    (shotSquares `intersect` shipSquares) == shipSquares -- All ships covered by shots?
  where
    shotSquares     = map (\(_,c,_) -> c) (shots b)
    shipSquares     = concatMap shipPlacementToCoords (placements b)

shipPlacementToCoords :: ShipPlacement -> [Coords]
shipPlacementToCoords (shipDimensions -> (sx,sy), (cx,cy), dir) =
  let cartesian xs ys = [(x,y) | x <- xs, y <- ys]
  in case dir of
    Downward  -> cartesian [cx..(cx+sx)] [cy..(cy+sy)]
    Rightward -> cartesian [cx..(cx+sy)] [cy..(cy+sx)]
