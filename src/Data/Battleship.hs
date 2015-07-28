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
  Dimensions,
  Ship,
  GameError(..),

  -- Headlining Library Functions
  mkShip,

  -- Helpers (for tests, but still safe)
  defaultShips,
  shipsFromList,

  -- TODO: Exporting record fields is /not/ safe.
  --       Need to rename fields to _foo, _barBaz, etc. and expose
  --       "read-only" helpers that call the internal underscored versions.
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

-- TODO: Consider splitting this into different groups; "preparatory" sort of
--       errors, and "game errors".
-- TODO: BoardNotReady and GameFinished are arguably something that should be
--       covered by making playing-an-unready-game and playing-a-finished-game
--       invalid by construction, eg.
--         data Board = EmptyBoard | PlacedBoard | InPlayBoard | FinishedBoard ...
--         data Game  = InPlayGame | FinishedGame ...
--       ... then only accepting an InPlayGame for attack and PlacedBoard for
--       mkGame or something.
data GameError =
                 InvalidShipDimensions Dimensions
               | InvalidShipInitial Char
               | InvalidShipName String
  deriving(Show,Eq)


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

