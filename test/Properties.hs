{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Battleship      as B

import           Control.Monad.Random ( evalRand,mkStdGen )
import           Data.Functor
import           Data.Either          ( isRight,rights )
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
-- wrapping the problematic ones with a one-second timeout.
wrap :: Property -> Property
wrap = within 1000000


-- Properties

-- TODO:
-- - placeShip overlapping existing piece is nothing
-- - placeShip not overlapping existing piece is a board with the placement
-- - placeShip of a (Positive Int,Positive Int)-sized ship in a (..,..)-sized board where the ship is smaller than the board and the placement is between 1 and totalX - shipX and <ditto for *Y>.
-- - Fire as many shots as there are squares; game should be finished.
-- - boardLargeEnough and mkRandomBoard should agree with each other.

prop_ValidBoardsHaveShipsPlacedInBounds :: Property
prop_ValidBoardsHaveShipsPlacedInBounds =
  forAll genPlacedBoard $ \eb -> wrap $
    let b                 = fromRight eb
        allCoordsInBounds = all $ B.coordsInBounds (B.boardDimensions b)
        shipInBounds      = allCoordsInBounds . B.shipPlacementToCoords
     in isRight eb ==> all shipInBounds $ B.placements b

prop_Derp :: Property
prop_Derp =
  forAll genDerp $ \x -> wrap $
    ((length x) `mod` (2 :: Int) == 0) ==> x === x
  where
    genDerp = listOf1 $ elements [1 :: Int]

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
