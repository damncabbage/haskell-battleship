module Data.Battleship where

import Data.Maybe
import Prelude    (Int,String,Bool(..),Show)

-- TODO: Custom Shows
type Position = (Int,Int)
data Ship     = Ship String (Int,Int)          deriving(Show)
data Player   = Player1 | Player2              deriving(Show)
data Board    = Board [(Player,Ship,Position)] deriving(Show)

-- First stab: branching, rather than encoding board states in types.

finished :: Board -> Bool
finished _ = False

play :: Board -> Player -> Ship -> Position -> Maybe Board
play b _ _ _ = case finished b of
                 True  -> Nothing
                 False -> Just b


dummyPlay = play (Board []) Player1 (Ship "Foo" (1,1)) (1,2)
