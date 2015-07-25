{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.Battleship                 as B

import           Test.Hspec
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Data.Functor
import           Data.Either
import           Control.Monad.Random
import           Debug.Trace

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



-- Little helpers for tests

-- HACK: This is kinda hideous. Something to never use outside of test code; at least
--       it's not a timebomb waiting to go off like it would be in "real" code.
fromRight :: Show a => Either a c -> c
fromRight e = either (error . show) id e

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

both :: (a -> b) -> (a,a) -> (b,b)
both f (a1,a2) = (f a1, f a2)


-- Generators

genPlacedBoard :: B.Dimensions -> [B.Ship] -> Gen (Either B.GameError B.Board)
genPlacedBoard dimensions ships = do
  generator <- mkStdGen <$> choose (minBound, maxBound)
  return $ (evalRand (B.mkRandomBoard dimensions ships) generator)

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
genShips dimensions = rights <$> listOf1 (genShip dimensions)

-- TODO: This is a little hideous; I *think* this is where ExceptT would be useful to bring in.
genNewGameWithBoardDimensions :: B.Dimensions -> Gen (Either B.GameError B.Game)
genNewGameWithBoardDimensions (w,h) = do
  ships  <- genShips (w,h)
  trace (show . length $ ships) (return ships)
  board1 <- genPlacedBoard (w,h) ships
  board2 <- genPlacedBoard (w,h) ships
  return $ do
    b1 <- board1
    b2 <- board2
    B.mkGame (B.Player1,b1) (B.Player2,b2)

genNewGame :: Gen (Either B.GameError B.Game)
genNewGame = sized $ \n -> do
  w      <- choose (1,1+n)
  h      <- choose (1,1+n)
  genNewGameWithBoardDimensions (w,h)

prop_RepeatedAttack =
  forAll (genNewGameWithBoardDimensions B.defaultBoardDimensions) $ \g x y ->
    --traceShow g $ (isRight g && B.coordsInBoardBounds (B.board1 $ fromRight g) (x,y))
    (isRight g && B.coordsInBoardBounds (B.board1 $ fromRight g) (x,y))
      ==> (B.attacksFromList (fromRight g) [(x,y), (1,1), (x,y)] === Left B.DuplicateShot)

main :: IO ()
main = hspec $ do
  let mkShips s = fromRight $ B.shipsFromList s

  -- B1:  1 2 3 4 5 6    B2:  1 2 3 4 5 6
  --    1 A · · · · ·       1 · · · B · ·
  --    2 A · · · · ·       2 · · · B · ·
  --    3 A B B B · ·       3 · · · B · ·
  --    4 A · · · · ·       4 · · · · · ·
  --    5 · · · · · ·       5 A A A A · ·
  --    6 · · · · · ·       6 · · · · · ·
  describe "big, dumb, happy-path run-through of the entire set of exported headlining functions" $ do
    let size  = (6,6)
    let ships = mkShips [ ("AA", 'A', (1,4))
                        , ("BB", 'B', (1,3))
                        ]
    let b1 = do
          eb <- B.mkEmptyBoard size ships
          B.placedBoardFromList eb $ zip3 ships [(1,1),(2,3)] [B.Downward,B.Rightward]
    let b2 = do
          eb <- B.mkEmptyBoard size ships
          B.placedBoardFromList eb $ zip3 ships [(1,5),(4,1)] [B.Rightward,B.Downward]
    let game xs = do
          pb1 <- b1
          pb2 <- b2
          g   <- B.mkGame (B.Player1,pb1) (B.Player2,pb2)
          B.attacksFromList g xs

    it "is a win for Player 1" $ do
      let p1Shots = [ (1,5), (2,5), (3,5), (4,5), (4,1), (4,2), (4,3) ]
      let p2Shots = [ (1,1), (2,1), (3,1), (4,1), (1,2), (2,2)        ]
      -- P1 sinks A, P2 only hit one of A --^
      --                                P1 sinks B, game over --^
      let finalGame = fromRight $ game $ merge p1Shots p2Shots
      B.attack finalGame (1,4) `shouldBe` Left B.GameFinished -- No more attacks allowed.
      B.finished finalGame     `shouldBe` True
      B.winner finalGame       `shouldBe` Just B.Player1
      (B.shots . B.board2 $ finalGame)      `shouldBe` map (\s -> (s,B.Hit)) p1Shots -- Shot results
      map fst (B.shots . B.board1 $  finalGame) `shouldBe` p2Shots -- Check at least that the shot coords are recorded.

    it "is a win for Player 2" $ do
      let p1Shots = [ (1,2), (2,3), (3,4), (4,5), (5,6), (2,1), (3,2) ]
      let p2Shots = [ (1,1), (1,2), (1,3), (1,4), (2,3), (3,3), (4,3) ]
      -- P2 sinks A, P1 only hit one of A --^
      --                                P2 sinks B, game over --^
      let finalGame = fromRight $ game $ merge p1Shots p2Shots
      B.attack   finalGame (1,4) `shouldBe` Left B.GameFinished -- No more attacks allowed.
      B.finished finalGame       `shouldBe` True
      B.winner   finalGame       `shouldBe` Just B.Player2
      (B.shots . B.board1 $ finalGame)      `shouldBe` map (\s -> (s,B.Hit)) p2Shots -- Shot results
      map fst (B.shots . B.board2 $ finalGame) `shouldBe` p1Shots -- Check at least that the shot coords are recorded.

--    describe "miscellaneous properties" $ do

--  describe "miscellaneous properties" $ do
--    -- prop "overlapping ships produces a failure result" $ do
--    --   \x -> showBoard x === show (fromJust (mkEmptyBoard (10,10) []))
--    prop "repeated shots by a single player are not allowed" $ do
--      \g x y -> (B.attacksFromList
--                  (fromRight g)
--                  [(x,y), (1,1), (x,y)]) === Left B.DuplicateShot


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
    let boardSetup p = do
          eboard <- B.mkEmptyBoard (6,6) ships
          B.placedBoardFromList eboard p
    let board1 p = fromRight $ boardSetup (defPlacements ++ p)
    let board2 p = fromRight $ boardSetup (defPlacements ++ p)

    it "disallows an incomplete board to be used" $ do
      let s = (ships !! 2, (2,1), B.Rightward)
      shouldBe (B.mkGame (B.Player1,board1 []) (B.Player2,board2 [s]))
               (Left $ B.BoardNotReady $ board1 [])
      shouldBe (B.mkGame (B.Player1,board1 [s]) (B.Player2,board2 []))
               (Left $ B.BoardNotReady $ board2 [])


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
      (B.shots . B.board1 $ fromRight $ game) `shouldSatisfy` (not . null)
      (B.shots . B.board2 $ fromRight $ game) `shouldSatisfy` (not . null)

    -- TODO: Fire as many shots as there are squares; game should be finished.
    -- TODO: Pairs of duplicate shots should result in a Left DuplicateShot
    -- TODO: BoardLargeEnough and mkRandomBoard should agree with each other.
    --
