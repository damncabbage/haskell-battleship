module Main where

import           Data.Battleship

import           Test.Hspec
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck
import           Data.Functor
import           Data.Maybe
import           Control.Applicative

-- TODO Properties:
-- - placeShip overlapping existing piece is nothing
-- - placeShip not overlapping existing piece is a board with the placement

-- TODO Arbitraries:
instance Arbitrary Board where
  arbitrary = (fromJust . mkEmptyBoard) <$> genDimensions
  -- TODO: arbitrary = (\x -> Board { size = x, placements = [], shots = [] }) <$> genDimensions

genDimensions :: Gen BoardDimensions
genDimensions = elements [(10,10)]

-- - blank board
-- - board with pre-placed pieces that do not overlap

showBoard :: Board -> String
showBoard = show

main :: IO ()
main = hspec $ do

  it "hello world" $ do
    "hello" `shouldBe` "hello"

  prop "quickcheck hello world" $
    \x -> (read . show) x === (x :: String)

  describe "placeShip" $ do
    prop "overlapping ships produces a failure result" $ do
      \x -> showBoard x === show (fromJust (mkEmptyBoard (10,10)))

