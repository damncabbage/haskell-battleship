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
  defaultBoardDimensions,
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

data Board = Board {
               boardDimensions :: Dimensions,
               placements      :: [ShipPlacement],
               shots           :: [(Coords,Result)],
               validShips      :: [Ship]
             }
data Game  = Game {
               currentPlayer :: Player,
               player1       :: Player,
               board1        :: Board,
               player2       :: Player,
               board2        :: Board
             } deriving(Show)

instance Show Board where
  show b =
    unlines $ [header] <> (map row [1..h])
    where
      (w,h)      = boardDimensions b
      itemWidth  = 1 + (length $ show h)
      item       = printf ("%-" <> (show itemWidth) <> "s")
      tlPadding  = replicate itemWidth ' '
      header     = tlPadding <> (concatMap (item . show) [1..w])
      row y      = concat [item (show y), (concatMap (\x -> cellItem (x,y)) [1..w])]
      cellItem c = item [cell c]
      findPlacement c =
        find (elem c . shipPlacementToCoords) (placements b)
      cell c
        | elem c (shotsCoords b) = 'x'
        | otherwise = maybe '·' (initial . shipFromPlacement) (findPlacement c)


-- TODO: One of the few cases I'd consider using a fromJust.
defaultShips :: [Ship]
defaultShips = fromMaybe [] $ shipsFromList
                 [ ("Carrier",    'C', (1,5))
                 , ("Battleship", 'B', (1,4))
                 , ("Submarine",  'S', (1,3))
                 , ("Cruiser",    'R', (1,3))
                 , ("Patrol",     'P', (1,2))
                 ]

defaultBoardDimensions :: Dimensions
defaultBoardDimensions = (10,10)

shipsFromList :: [(String,Char,Coords)] -> Maybe [Ship]
shipsFromList = sequence . map (\(n,i,p) -> mkShip n i p)

mkShip :: String -> Char -> (Int,Int) -> Maybe Ship
mkShip n i d
  | vDims d && vInitial i && vName n = Just Ship { name = n, initial = i, shipDimensions = d }
  | otherwise                        = Nothing
  where
    vDims (x,y) = x > 0 && y > 0
    vInitial i  = elem i ['A'..'Z']
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
  | p1 /= p2  = Just Game { currentPlayer = p1 -- Just default to the first, whatever it is.
                          , player1 = p1
                          , board1  = b1
                          , player2 = p2
                          , board2  = b2
                          }
  | otherwise = Nothing

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

attack :: Game -> Coords -> Maybe Game
attack g c
  | inBounds board c && notRepeated && (not $ finished g) =
      Just appendedShotAndSwappedPlayer
  | otherwise = Nothing
  where
    appendedShotAndSwappedPlayer
      | (currentPlayer g) == (player1 g) = g { board1 = appendShot, currentPlayer = player2 g }
      | (currentPlayer g) == (player2 g) = g { board2 = appendShot, currentPlayer = player1 g }
    inBounds (boardDimensions -> (bw,bh)) (cx,cy) =
      (cx >= 1) && (cx <= bw) &&
      (cy >= 1) && (cy <= bh)
    notRepeated = notElem c (shotsCoords board)
    ships       = placements board
    result      = maybe Miss Hit (find (elem c . shipPlacementToCoords) ships)
    isHit       = case result of (Hit _) -> True; Miss -> False
    appendShot  = board { shots = (shots board) <> [(c,result)] }
    board
      | (currentPlayer g) == (player1 g) = board1 g
      | (currentPlayer g) == (player2 g) = board2 g

attacksFromList :: Game -> [Coords] -> Maybe Game
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
    shotSquares = shotsCoords b
    shipSquares = concatMap shipPlacementToCoords (placements b)

shipPlacementToCoords :: ShipPlacement -> [Coords]
shipPlacementToCoords (shipDimensions -> (sx,sy), (cx,cy), dir) =
  let cartesian xs ys = [(x,y) | x <- xs, y <- ys]
  in case dir of
    Downward  -> cartesian [cx..(cx+sx-1)] [cy..(cy+sy-1)]
    Rightward -> cartesian [cx..(cx+sy-1)] [cy..(cy+sx-1)]

shotsCoords :: Board -> [Coords]
shotsCoords = map fst . shots

shipFromPlacement :: ShipPlacement -> Ship
shipFromPlacement (ship,_,_) = ship

-- TODO: Depth-first search
--       https://monadmadness.wordpress.com/2014/11/10/purely-functional-graph-search-algorithms/
data Graph a = Node a [Graph a]
instance Show a => Show (Graph a) where
  show (Node x []) = "Node " <> (show x) <> " []"
  show (Node x _)  = "Node " <> (show x) <> " [...]"

sampleGraph = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]

--       (b -> s -> [b])              -> Graph b     -> Maybe b
--dfs :: (Board -> [Ship] -> [Board]) -> Graph Board -> Maybe Board
--dfs p (Node b bs)
--  | null (p b) = Just b
--  | otherwise  = fmap (b:) . msum . map (dfs p) $ bs

firstWalk :: (b -> p -> [b]) -> [p] -> Graph b -> Maybe b
firstWalk _ []     (Node b [])     = Just b
firstWalk _ (_:_)  (Node b [])     = Nothing
firstWalk f (s:ss) (Node b (bf:_)) = firstWalk f ss (Node bf (step bf s)) -- Choose the first


step :: Board -> Ship -> [Board]
step board ship =
  catMaybes $ map (placeShip board) permutations
  where
    (w,h) = boardDimensions board
    permutations :: [ShipPlacement]
    permutations = [ (ship, (1,1), Downward) -- TODO
                   , (ship, (2,1), Downward)
                   ]
-- eg.
-- step (fromJust $ mkEmptyBoard defaultBoardDimensions defaultShips) defaultShips
