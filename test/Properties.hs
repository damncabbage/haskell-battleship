{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Battleship      as B

import           Control.Monad.Random ( evalRand,mkStdGen )
import           Data.Functor
import           Data.Either          ( isRight,rights )
import           Data.List.Unique     ( repeated )
import           Test.QuickCheck
import           TestHelper           ( fromRight )


-- Generators

genPlacedBoardOf :: B.Dimensions -> [B.Ship] -> Gen (Either B.GameError B.Board)
genPlacedBoardOf dims ships = do
  generator <- mkStdGen <$> choose (minBound, maxBound)
  return $ (evalRand (B.mkRandomBoard dims ships) generator)

genPlacedBoard :: Gen (Either B.GameError B.Board)
genPlacedBoard = do
  dims  <- genBoardDimensions
  ships <- genShips dims
  genPlacedBoardOf dims ships

-- Range: 3 - 15. A bit limited, but it'll stop
-- the generation of useless micro- and mega-boards.
genBoardDimensions :: Gen (B.Dimensions)
genBoardDimensions = sized $ \n -> do
  let looped = n `mod` 15
      lower  = 3
      range = (lower,max lower looped)
  w <- choose range
  h <- choose range
  return $ (w,h)

genShip :: B.Dimensions -> Gen (Either B.GameError B.Ship)
genShip (x,y) = do
  w       <- frequency [(5, return 1),    (1, choose(1,maxDim))]
  h       <- frequency [(5, choose(1,6)), (1, choose(1,maxDim))]
  name    <- listOf1 $ elements latinChars
  initial <- choose ('A','Z')
  return $ B.mkShip name initial (w,h)
  where
    maxDim     = maximum [x,y]
    latinChars = concat [[' '],['A'..'Z'],['a'..'z'],['1'..'9']]

genShips :: B.Dimensions -> Gen [B.Ship]
genShips dims = sized $ \n -> do
  let looped = n `mod` 10
  k <- choose (1,1 `max` looped)
  rights <$> vectorOf k (genShip dims)

-- TODO: I *think* this is where EitherT would be useful to bring in.
genNewGameWithBoardDimensions :: B.Dimensions -> Gen (Either B.GameError B.Game)
genNewGameWithBoardDimensions (w,h) = do
  ships  <- genShips (w,h)
  board1 <- genPlacedBoardOf (w,h) ships
  board2 <- genPlacedBoardOf (w,h) ships
  return $ do
    b1 <- board1
    b2 <- board2
    B.mkGame (B.Player1,b1) (B.Player2,b2)

genNewGame :: Gen (Either B.GameError B.Game)
genNewGame = do
  dims <- genBoardDimensions
  genNewGameWithBoardDimensions dims


-- Helpers

-- HACK TODO: There are, at the moment, some properties (eg. prop_RepeatedAttack)
-- that hang. I think it might be mkRandomBoard doing a full traversal, but
-- until I can dig into this and get profiling successfully compiled in, I'm
-- wrapping the problematic ones with a five-second timeout.
wrap :: Property -> Property
wrap = within 5000000


-- Properties

-- TODO:
{-
 - mkShip
   - mkShip with <= 0 dimensions is Left InvalidShipDimensions
   - mkShip with an invalid character is Left InvalidShipInitial
   - mkShip with an empty name is Left InvalidShipName
   - mkShip used via the generator is a Right Ship

 - mkEmptyBoard
   - mkEmptyBoard with no ships should be Left NoShips
   - mkEmptyBoard with a dimension and set of ships not boardLargeEnoughForShips is Left InvalidBoardDimensions
   - mkEmptyBoard with valid dimensions and ships is Right Board

 - mkRandomBoard
   - boardLargeEnough and mkRandomBoard should agree with each other.

 - placeship
   - placeShip overlapping existing piece is Left OverlapsPlacedShip
   - placeShip with any shipPlacementToCoords resulting in a not-coordsInBounds item is Left OutOfBoundsShip
   - placeShip not overlapping existing piece or out of bounds is a Board with the new placement

 - mkGame

 - attack
   - attack with a shot out of bounds is Left OutOfBoundsShot
   - attack on a finished game is Left GameFinished
   - A valid attack is a game with the shot recorded and the player swapped.

 - finished
   - Fire as many shots as there are squares on the Board; the Game should have been finished.
   - A game with fewer shots for a player than there are coords for opposing ships is not finished.

 - winner
   - attacksFromList with Player1 firing shots that include the set of coords, and Player2 not, is Just Player1.
   - A game that is not finished will is winner == Nothing
-}

-- If the generated board has been declared valid, then no ships should be out of bounds.
prop_ValidBoardsHaveShipsPlacedInBounds :: Property
prop_ValidBoardsHaveShipsPlacedInBounds =
  forAll genPlacedBoard $ \eb -> wrap $
    let b                 = fromRight eb
        allCoordsInBounds = all $ B.coordsInBounds (B.boardDimensions b)
        shipInBounds      = allCoordsInBounds . B.shipPlacementToCoords
     in isRight eb ==> all shipInBounds $ B.placements b

-- Ditto for overlapping ships.
prop_ValidBoardsHaveNoOverlappingShips :: Property
prop_ValidBoardsHaveNoOverlappingShips =
  forAll genPlacedBoard $ \eb -> wrap $
    let b               = fromRight eb
        allCoords       = concatMap B.shipPlacementToCoords
     in isRight eb ==> repeated (allCoords $ B.placements b) === []

prop_RepeatedAttack :: Property
prop_RepeatedAttack =
  forAll genNewGame $ \eg x y -> wrap $
    let g           = fromRight eg
        shotOnBoard = B.coordsInBounds (B.boardDimensions . B.board2 $ g) (x,y)
     in isRight eg && shotOnBoard ==>
        (B.attacksFromList g [(x,y), (1,1), (x,y)] === Left B.DuplicateShot)


return [] -- What.
main :: IO Bool
main = $quickCheckAll
