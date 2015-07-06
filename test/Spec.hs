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

-- HACK: This is kinda hideous. Something to never use outside of test code; at least
--       it's not a timebomb waiting to go off like it would be in "real" code.
fromRight e = either (error . show) id e

main :: IO ()
main = hspec $ do
  let mkShips s = fromRight $ B.shipsFromList s

  describe "boardLargeEnoughForShips" $ do
    it "uses the longest ship and ship area to determine the minimum size" $ do
      let ships = mkShips [ ("Carrier",    'C', (1,5))
                          , ("Battleship", 'B', (1,4))
                          , ("Submarine",  'S', (1,3))
                          , ("Cruiser",    'R', (1,3))
                          , ("Patrol",     'P', (1,2))
                          ]
      (B.boardLargeEnoughForShips (5,4) ships) `shouldBe` True
      (B.boardLargeEnoughForShips (4,3) ships) `shouldBe` False
      (B.boardLargeEnoughForShips (5,2) ships) `shouldBe` False


  describe "placeShip" $ do
    let ships = mkShips [ ("AA", 'A', (1,5))
                        , ("BB", 'B', (2,4))
                        , ("CC", 'C', (1,4))
                        ]
    let defPlacements = [ (ships !! 0, (1,1), B.Downward)
                        , (ships !! 1, (2,3), B.Rightward)
                        ]
    let boardWith p = do
          board <- B.mkEmptyBoard (6,6) ships
          B.placedBoardFromList board p

    --   1 2 3 4 5 6
    -- 1 A · · · · ·
    -- 2 A · · · · ·
    -- 3 A B B B B ·
    -- 4 A B B B B ·
    -- 5 A · · · · ·
    -- 6 · · · · · ·
    it "allows non-overlapping placement" $ do
      let p = defPlacements ++ [(ships !! 2, (6,1), B.Downward)]
      (B.placements $ fromRight $ boardWith p) `shouldBe` p

    it "disallows overlapping placement" $ do
      let p = defPlacements ++ [(ships !! 2, (2,1), B.Downward)] -- Overlapping Ship B
      boardWith p `shouldBe` Left B.OverlapsPlacedShip

    -- prop "overlapping ships produces a failure result" $ do
    --   \x -> showBoard x === show (fromJust (mkEmptyBoard (10,10) []))

  describe "mkGame" $ do
    let ships = mkShips [ ("AA", 'A', (1,5))
                        , ("BB", 'B', (2,4))
                        , ("CC", 'C', (1,4))
                        ]
    let defPlacements = [ (ships !! 0, (1,1), B.Downward)
                        , (ships !! 1, (2,3), B.Rightward)
                        ]
    let gameSetup p1 p2 = do
          eboard1 <- B.mkEmptyBoard (6,6) ships
          eboard2 <- B.mkEmptyBoard (6,6) ships
          pboard1 <- B.placedBoardFromList eboard1 p1
          pboard2 <- B.placedBoardFromList eboard2 p2
          B.mkGame (B.Player1,pboard1) (B.Player2,pboard2)

    it "disallows an incomplete board to be used" $ do
       "TODO" `shouldBe` "DONE"
       -- (gameSetup defPlacements defPlacements) `shouldBe` (Left $ B.BoardNotReady ...)



  describe "attack" $ do
    let ships = mkShips [ ("AA", 'A', (1,5))
                        , ("BB", 'B', (2,4))
                        , ("CC", 'C', (1,4))
                        ]
    let defPlacements = [ (ships !! 0, (1,1), B.Downward)
                        , (ships !! 1, (2,3), B.Rightward)
                        ]
    let gameSetup = do
          eboard1 <- B.mkEmptyBoard (6,6) ships
          eboard2 <- B.mkEmptyBoard (6,6) ships
          pboard1 <- B.placedBoardFromList eboard1 (defPlacements ++ [(ships !! 2, (6,1), B.Downward)])
          pboard2 <- B.placedBoardFromList eboard2 (defPlacements ++ [(ships !! 2, (6,1), B.Downward)])
          B.mkGame (B.Player1,pboard1) (B.Player2,pboard2)

    let initialGame = fromRight gameSetup

    it "allows valid shots" $ do
      let shots = [ (3,3)
                  , (3,3)
                  ]
      let game = B.attacksFromList initialGame shots
      -- TODO: Not a real test
      (B.shots . B.board1 $ fromRight $ game) `shouldSatisfy` (not . null)
      (B.shots . B.board2 $ fromRight $ game) `shouldSatisfy` (not . null)

    it "is an example game set of shots with some debug prints to rip out" $ do
      let shots = [ (3,3)
                  , (3,4)
                  , (5,6)
                  , (3,5)
                  , (6,3)
                  , (5,5)
                  ]
      let game = B.attacksFromList initialGame shots
      -- TODO: Not a real test
      print (B.board1 $ fromRight game)
      print (B.board2 $ fromRight game)
      (B.shots . B.board1 $ fromRight $ game) `shouldSatisfy` (not . null)
      (B.shots . B.board2 $ fromRight $ game) `shouldSatisfy` (not . null)

    -- TODO: Fire as many shots as there are squares; game should be finished.
    -- TODO: Pairs of duplicate shots should result in a Left DuplicateShot
    -- TODO: BoardLargeEnough and mkRandomBoard should agree with each other.
    --
