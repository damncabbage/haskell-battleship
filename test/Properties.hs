{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Battleship      as B

import           Control.Applicative
import           Control.Monad.Random ( evalRand,mkStdGen )
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


-- Helpers

-- HACK TODO: There are, at the moment, some properties (eg. prop_RepeatedAttack)
-- that hang. I think it might be mkRandomBoard doing a full traversal, but
-- until I can dig into this and get profiling successfully compiled in, I'm
-- wrapping the problematic ones with a five-second timeout.
wrap :: Property -> Property
wrap = within 5000000


-- Properties

-- If the generated board has been declared valid, then no ships should be out of bounds.
prop_ValidBoardsHaveShipsPlacedInBounds :: Property
prop_ValidBoardsHaveShipsPlacedInBounds =
  forAll genPlacedBoard $ \eb -> wrap $
    let b                 = fromRight eb
        allCoordsInBounds = all $ B.coordsInBounds (B.boardDimensions b)
        shipInBounds      = allCoordsInBounds . B.shipPlacementToCoords
     in isRight eb ==> all shipInBounds $ B.placements b

-- Same for overlapping ships.
prop_ValidBoardsHaveNoOverlappingShips :: Property
prop_ValidBoardsHaveNoOverlappingShips =
  forAll genPlacedBoard $ \eb -> wrap $
    let b               = fromRight eb
        allCoords       = concatMap B.shipPlacementToCoords
     in isRight eb ==> repeated (allCoords $ B.placements b) === []

-- Have all the ships been placed on the Board?
prop_ValidBoardsHaveAllShips :: Property
prop_ValidBoardsHaveAllShips =
  forAll genPlacedBoard $ \eb -> wrap $
    let b           = fromRight eb
        givenShips  = B.validShips b
        placedShips = map B.shipFromPlacement $ B.placements b
     in isRight eb ==> givenShips === placedShips


return [] -- What.
main :: IO Bool
main = $quickCheckAll
