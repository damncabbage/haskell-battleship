module Main where

import qualified Data.Battleship                 as B

import           Test.Hspec
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck
import           Data.Functor
import           Data.Maybe
import           Control.Applicative
import           Control.Monad

-- TODO Properties:
-- - placeShip overlapping existing piece is nothing
-- - placeShip not overlapping existing piece is a board with the placement
-- - placeShip of a (Positive Int,Positive Int)-sized ship in a (..,..)-sized board where the ship is smaller than the board and the placement is between 1 and totalX - shipX and <ditto for *Y>.

-- TODO Arbitraries:
-- instance Arbitrary Board where
--   arbitrary = (fromJust . mkEmptyBoard) <$> genDimensions
--   -- TODO: arbitrary = (\x -> Board { size = x, placements = [], shots = [] }) <$> genDimensions
--
-- genDimensions :: Gen Dimensions
-- genDimensions = elements [(10,10)]
--
-- genShips :: Gen [Ship]
-- genShips = error "TODO"

-- - blank board
-- - board with pre-placed pieces that do not overlap

showBoard :: B.Board -> String
showBoard = show

main :: IO ()
main = hspec $ do

  it "hello world" $ do
    "hello" `shouldBe` "hello"

  prop "quickcheck hello world" $
    \x -> (read . show) x === (x :: String)

  --describe "placeShip" $ do
  --  prop "overlapping ships produces a failure result" $ do
  --    \x -> showBoard x === show (fromJust (mkEmptyBoard (10,10) []))

  describe "boardLargeEnoughForShips" $ do
    it "uses the longest ship and ship area to determine the minimum size" $ do
      let ships = fromJust $ B.shipsFromList [
                               ("Carrier",    (1,5)),
                               ("Battleship", (1,4)),
                               ("Submarine",  (1,3)),
                               ("Cruiser",    (1,2)),
                               ("Patrol",     (1,1))
                             ]
      (B.boardLargeEnoughForShips (5,3) ships) `shouldBe` True
      (B.boardLargeEnoughForShips (4,3) ships) `shouldBe` False
      (B.boardLargeEnoughForShips (5,2) ships) `shouldBe` False

  describe "placeShip" $ do
    let ships = fromJust $ B.shipsFromList [
                             ("A", (1,5)),
                             ("B", (2,4)),
                             ("C", (1,4))
                           ]
    let initialBoard  = fromJust $ B.mkEmptyBoard (6,6) ships
    let boardWith     = B.boardFromList initialBoard
    let defPlacements = [
                          (ships !! 0, (1,1), B.Downward),
                          (ships !! 1, (2,3), B.Rightward)
                        ]


    --   1 2 3 4 5 6
    -- 1 A · · · · ·
    -- 2 A · · · · ·
    -- 3 A B B B B ·
    -- 4 A B B B B ·
    -- 5 A · · · · ·
    -- 6 · · · · · ·
    it "allows non-overlapping placement" $ do
      let p = defPlacements ++ [(ships !! 2, (6,1), B.Downward)]
      (maybe [] (B.placements) (boardWith p)) `shouldBe` p

    it "disallows overlapping placement" $ do
      let p = defPlacements ++ [(ships !! 2, (2,1), B.Downward)] -- Overlapping Ship B
      (maybe [] (B.placements) (boardWith p)) `shouldBe` []
