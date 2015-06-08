module Data.Battleship where

import Data.Maybe

-- TODO: Custom Shows
type Position = (Int,Int)
data Ship     = Ship String (Int,Int)          deriving(Show)
data Player   = Player1 | Player2              deriving(Show)
data Board    = Board [(Player,Ship,Position)] deriving(Show)

defaultShips = map (\(n,p) -> Ship n p) [
                 ("Carrier",    (1,5)),
                 ("Battleship", (1,4)),
                 ("Submarine",  (1,3)),
                 ("Cruiser",    (1,2)),
                 ("Patrol",     (1,1)),
               ]

-- First stab: branching, rather than encoding board states in types.

finished :: Board -> Bool
finished _ = False

play :: Board -> Player -> Ship -> Position -> Maybe Board
play b _ _ _ = case finished b of
                 True  -> Nothing
                 False -> Just b


dummyPlay = play (Board []) Player1 (Ship "Foo" (1,1)) (1,2)
