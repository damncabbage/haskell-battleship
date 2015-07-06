{-# LANGUAGE ViewPatterns, NoImplicitPrelude #-}

-- TODO: Positive a / getPositive for generation of coords
-- TODO: Break Board-, Ship- and Game-related functions out into their own modules;
--       things are starting to get unwieldy names trying to make clear what they
--       relate to.
-- TODO: Switch to (Either <SpecificErrorSumType> a) from all the Maybes; it's a
--       black box for users right now.
-- TODO: Consider a different way of doing ShipPlacement without the terrible
--       (_,x,_)-style matching to extract parts.
-- TODO: Consider whether we want the Ship that's been hit stored as part of the
--       Result type or not; it is now, but may be "information leakage" of sorts.
-- TODO: Rewrite the history of these commits so nobody will find out that I'm
--       secretly terrible at all of this.

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
  GameError(..),

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

import Data.Maybe             ( Maybe(Just,Nothing),isJust,maybe )
import Data.Either            ( either,rights )
import Data.Monoid            ( (<>),mconcat )
import Data.List              ( any,concat,concatMap,elem,find,intersect,map,maximum,notElem,null,sum )
import Control.Monad          ( Monad,foldM,return,sequence )
import Control.Monad.Random   ( MonadRandom )
import System.Random.Shuffle  ( shuffleM )
import Text.Printf            ( printf )
import Prelude                ( Bool,Char,Eq,Int,Show,String,Either(Left,Right)
                              , ($),(&&),(*),(+),(-),(.),(<),(<=),(==),(/=),(>),(>=),(||)
                              , const,error,foldr,fst,id,length,max,not,otherwise,replicate,show,snd,unlines
                              )

data Direction     = Downward | Rightward     deriving(Show,Eq)
data Result        = Hit ShipPlacement | Miss deriving(Show,Eq)
data Player        = Player1 | Player2        deriving(Show,Eq)
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
data Game  = Game {
               currentPlayer :: Player,
               player1       :: Player,
               board1        :: Board,
               player2       :: Player,
               board2        :: Board
             } deriving(Show,Eq)

-- TODO: Consider splitting this into different groups; "preparatory" sort of
--       errors, and "game errors".
--       BoardNotReady and GameFinished are arguably something that should be
--       covered by making playing-an-unready-game and playing-a-finished-game
--       invalid by construction, eg.
--         data Board = EmptyBoard | PlacedBoard | InPlayBoard | FinishedBoard ...
--         data Game  = InPlayGame | FinishedGame ...
--       ... then only accepting an InPlayGame for attack and PlacedBoard for
--       mkGame or something.
data GameError = InvalidDimensions Dimensions
               | InvalidInitial Char
               | InvalidName String
               | NoShips
               | DuplicatePlayers Player
               | DuplicateShot
               | OutOfBounds
               | OverlapsPlacedShip
               | BoardNotReady Board
               | GameFinished
  deriving(Show,Eq)

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


-- One of the few cases I'd consider using a fromJust; this not working is a
-- border-line program bug.
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
  | not $ vDims d        = Left $ InvalidDimensions d
  | notElem i ['A'..'Z'] = Left $ InvalidInitial i
  | length n <= 0        = Left $ InvalidName n
  | otherwise            = Right $ Ship { name = n, initial = i, shipDimensions = d }
  where
    vDims (x,y) = x > 0 && y > 0

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

mkEmptyBoard :: Dimensions -> [Ship] -> Either GameError Board
mkEmptyBoard d s
  | null s                             = Left $ NoShips
  | not (boardLargeEnoughForShips d s) = Left $ InvalidDimensions d
  | otherwise =
      Right Board { boardDimensions = d, placements = [], shots = [], validShips = s }

mkRandomBoard :: MonadRandom m => Dimensions -> [Ship] -> m (Either GameError Board)
mkRandomBoard dims ships = do
  -- TODO: Learn how to use monad transformers. Use EitherT+MaybeT here.
  either (\e -> return $ Left e)
         (\board -> findWith board)
         (mkEmptyBoard dims ships)
  where
    findWith board = do
      nb <- depthFirstGraphSearch ordering
                                  (\b -> length(placements(b)) == length(validShips(b)))
                                  (graph board)
      return $ maybe (Left $ InvalidDimensions dims) -- Arguably a bug in boardLargeEnoughForShips
                     (Right)
                     (nb)
    graph b = placementsGraph placementStep ships b
    ordering = shuffleM

mkGame :: (Player,Board) -> (Player,Board) -> Either GameError Game
mkGame (p1,b1) (p2,b2)
  | p1 == p2      = Left $ DuplicatePlayers p1
  | incomplete b1 = Left $ BoardNotReady b1
  | incomplete b2 = Left $ BoardNotReady b1
  | otherwise     = Right $ Game { currentPlayer = p1 -- Just default to the first, whatever it is.
                                 , player1 = p1
                                 , board1  = b1
                                 , player2 = p2
                                 , board2  = b2
                                 }
  where
    incomplete b = (validShips b) /= (map shipFromPlacement $ placements b)

placeShip :: Board -> ShipPlacement -> Either GameError Board
placeShip b p
  | not $ inBounds (boardDimensions b) p = Left OutOfBounds
  | (any (overlapping p) (placements b)) = Left OverlapsPlacedShip
  | otherwise = Right b { placements = placements b <> [p] }
  where
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
    topLeft (_,sp,_) = sp
    bottomRight (shipDimensions -> (sx,sy), (cx,cy), dir) =
      case dir of
        Downward  -> (cx + sx, cy + sy)
        Rightward -> (cx + sy, cy + sx)

placedBoardFromList :: Board -> [ShipPlacement] -> Either GameError Board
placedBoardFromList = foldM placeShip

attack :: Game -> Coords -> Either GameError Game
attack g c
  | not $ inBounds board c = Left OutOfBounds
  | repeated c             = Left DuplicateShot
  | finished g             = Left GameFinished
  | otherwise              = Right appendedShotAndSwappedPlayer
  where
    appendedShotAndSwappedPlayer =
      player1Or2 (g { board1 = appendedShot, currentPlayer = player2 g })
                 (g { board2 = appendedShot, currentPlayer = player1 g })
    inBounds (boardDimensions -> (bw,bh)) (cx,cy) =
      (cx >= 1) && (cx <= bw) &&
      (cy >= 1) && (cy <= bh)
    board        = player1Or2 (board1 g) (board2 g)
    repeated s   = elem s (shotsCoords board)
    result       = maybe Miss Hit (find (elem c . shipPlacementToCoords) (placements board))
    appendedShot = board { shots = (shots board) <> [(c,result)] }
    player1Or2 fp1 fp2
      | (currentPlayer g) == (player1 g) = fp1
      | (currentPlayer g) == (player2 g) = fp2
      | otherwise                        = error "BUG: currentPlayer is neither player1 or player2"

attacksFromList :: Game -> [Coords] -> Either GameError Game
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


-- Depth-first search bits
data Graph a = Node a [Graph a]
instance Show a => Show (Graph a) where
  show (Node x []) = "Node " <> (show x) <> " []"
  show (Node x _)  = "Node " <> (show x) <> " [...]"

placementsGraph :: (b -> s -> [b]) -> [s] -> b -> Graph b
placementsGraph _ []     b = Node b []
placementsGraph f (s:ss) b = Node b (map (placementsGraph f ss) (f b s))

placementStep :: Board -> Ship -> [Board]
placementStep board ship =
  rights $ map (placeShip board) permutations
  where
    (w,h) = boardDimensions board
    permutations :: [ShipPlacement]
    permutations = [(ship,(x,y),d) | d <- [Downward,Rightward], y <- [1..h], x <- [1..w]]

-- A heavily-modified version of the dfs search from this post:
-- https://monadmadness.wordpress.com/2014/11/10/purely-functional-graph-search-algorithms/
depthFirstGraphSearch :: Monad m => ([Graph b] -> m [Graph b]) -> (b -> Bool) -> Graph b -> m (Maybe b)
depthFirstGraphSearch ord pred (Node x xs)
  | pred x    = return $ Just x
  | null xs   = return $ Nothing
  | otherwise = do
      oxs <- ord xs
      select (depthFirstGraphSearch ord pred) oxs
  where
    select _     []     = return Nothing
    select check (b:bs) = do
      cb <- (check b)
      if isJust cb
        then return cb
        else select check bs
