{-# LANGUAGE ViewPatterns, NoImplicitPrelude #-}

-- TODO: Positive a / getPositive for generation of coords
-- TODO: Break Board-, Ship- and Game-related functions out into their own modules;
--       things are starting to get unwieldy names trying to make clear what they
--       relate to.
-- TODO: Consider a different way of doing ShipPlacement without the terrible
--       (_,x,_)-style matching to extract parts.
-- TODO: Rewrite the history of these commits so nobody will find out that I'm
--       secretly terrible at all of this.

module Data.Battleship (
  -- Types and (safe) constructors
  Board,
  Coords,
  Dimensions,
  Direction(..),
  Ship,
  ShipPlacement,
  GameError(..),

  -- Headlining Library Functions
  mkShip,
  mkEmptyBoard,
  placeShip,

  -- Helpers (for tests, but still safe)
  defaultShips,
  defaultBoardDimensions,
  attacksFromList,
  boardFinished,
  boardLargeEnoughForShips,
  coordsInBounds,
  placedBoardFromList,
  shipPlacementToCoords,
  shipFromPlacement,
  shipsFromList,

  -- TODO: Exporting record fields is /not/ safe.
  --       Need to rename fields to _foo, _barBaz, etc. and expose
  --       "read-only" helpers that call the internal underscored versions.
  boardDimensions,
  placements,
  shots,
  validShips,

  name,
  initial,
  shipDimensions
) where

import Data.Maybe             ( Maybe(Just,Nothing),isJust,maybe )
import Data.Either            ( either,rights )
import Data.Monoid            ( (<>),mconcat )
import Data.List              ( any,concat,concatMap,elem,find,intersect,map,maximum,notElem,null,sum )
import Control.Monad          ( Monad,foldM,return,sequence )
import Text.Printf            ( printf )
import Prelude                ( Bool,Char,Eq,Int,Show,String,Either(Left,Right)
                              , ($),(&&),(*),(+),(-),(.),(<),(<=),(==),(/=),(>),(>=),(||)
                              , const,error,foldr,fst,id,length,max,not,otherwise,replicate,show,snd,unlines
                              )

data Direction     = Downward | Rightward     deriving(Show,Eq)
data Ship          = Ship {
                       name           :: String,
                       initial        :: Char,
                       shipDimensions :: Dimensions
                     } deriving(Show,Eq)

type Dimensions    = (Int,Int)
type Coords        = (Int,Int)
type ShipPlacement = (Ship,Coords,Direction)

data Board = Board {
               boardDimensions :: Dimensions,
               placements      :: [ShipPlacement],
               shots           :: [(Coords,Result)],
               validShips      :: [Ship]
             } deriving(Eq)

-- TODO: Consider splitting this into different groups; "preparatory" sort of
--       errors, and "game errors".
-- TODO: BoardNotReady and GameFinished are arguably something that should be
--       covered by making playing-an-unready-game and playing-a-finished-game
--       invalid by construction, eg.
--         data Board = EmptyBoard | PlacedBoard | InPlayBoard | FinishedBoard ...
--         data Game  = InPlayGame | FinishedGame ...
--       ... then only accepting an InPlayGame for attack and PlacedBoard for
--       mkGame or something.
data GameError = InvalidBoardDimensions Dimensions [Ship]
               | InvalidShipDimensions Dimensions
               | InvalidShipInitial Char
               | InvalidShipName String
               | OutOfBoundsShip
               | NoShips
  deriving(Show,Eq)


-- TODO: Boy, this is a mess.
instance Show Board where
  show b =
    unlines $ ["", header] <> (map row [1..h])
    where
      (w,h)       = boardDimensions b
      itemWidth d = 1 + (length $ show d)
      item d      = printf ("%-" <> (show $ itemWidth d) <> "s")
      tlPadding   = replicate (itemWidth h) ' '
      header      = tlPadding <> (concatMap (item w . show) [1..w])
      row y       = concat [item h (show y), (concatMap (\x -> cellItem (x,y)) [1..w])]
      cellItem c  = item w [cell c]
      findPlacement c =
        find (elem c . shipPlacementToCoords) (placements b)
      cell c
        | elem c (shotsCoords b) = 'x'
        | otherwise = maybe '·' (initial . shipFromPlacement) (findPlacement c)


-- One of the few cases I'd consider using a fromJust; this not working is
-- basically a program bug.
defaultShips :: [Ship]
defaultShips = either (const []) (id) $ shipsFromList
                 [ ("Carrier",    'C', (1,5))
                 , ("Battleship", 'B', (1,4))
                 , ("Submarine",  'S', (1,3))
                 , ("Cruiser",    'R', (1,3))
                 , ("Patrol",     'P', (1,2))
                 ]

defaultBoardDimensions :: Dimensions
defaultBoardDimensions = (10,10)

shipsFromList :: [(String,Char,Coords)] -> Either GameError [Ship]
shipsFromList = sequence . map (\(n,i,p) -> mkShip n i p)

mkShip :: String -> Char -> (Int,Int) -> Either GameError Ship
mkShip n i d
  | not $ vDims d        = Left $ InvalidShipDimensions d
  | notElem i ['A'..'Z'] = Left $ InvalidShipInitial i
  | length n <= 0        = Left $ InvalidShipName n
  | otherwise            = Right $ Ship { name = n, initial = i, shipDimensions = d }
  where
    vDims (x,y) = x > 0 && y > 0

-- An inexpensive first-pass filter; later on, mkRandomBoard (for example) or some other
-- board-placement function will end up doing a more accurate (but expensive) check as they
-- attempt to place all the ships given to them.
boardLargeEnoughForShips :: Dimensions -> [Ship] -> Bool
boardLargeEnoughForShips (x,y) ships =
  (largestDim >= longestShip) && (x * y >= shipsArea)
  where
    largestDim  = max x y
    longestShip = (maximum . mconcat . map (pairToList . shipDimensions)) ships
    shipsArea   = (sum . map (multList . pairToList . shipDimensions)) ships
    multList    = foldr (*) 1
    pairToList (a,b) = [a,b]

mkEmptyBoard :: Dimensions -> [Ship] -> Either GameError Board
mkEmptyBoard d s
  | null s                             = Left $ NoShips
  | not (boardLargeEnoughForShips d s) = Left $ InvalidBoardDimensions d s
  | otherwise =
      Right Board { boardDimensions = d, placements = [], shots = [], validShips = s }

placeShip :: Board -> ShipPlacement -> Either GameError Board
placeShip b p
  | not $ placementInBounds (boardDimensions b) p = Left OutOfBoundsShip
  | (any (overlapping p) (placements b))          = Left OverlapsPlacedShip
  | otherwise = Right b { placements = placements b <> [p] }
  where
    placementInBounds (bw,bh) (shipDimensions -> (sx,sy), (cx,cy), dir) =
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
    topLeft (_,sp,_) = sp
    bottomRight (shipDimensions -> (sx,sy), (cx,cy), dir) =
      case dir of
        Downward  -> (cx + sx, cy + sy)
        Rightward -> (cx + sy, cy + sx)

placedBoardFromList :: Board -> [ShipPlacement] -> Either GameError Board
placedBoardFromList = foldM placeShip

coordsInBounds :: Dimensions -> Coords -> Bool
coordsInBounds (bw,bh) (cx,cy) =
  (cx >= 1) && (cx <= bw) &&
  (cy >= 1) && (cy <= bh)

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
