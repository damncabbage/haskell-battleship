module Main where

import qualified Data.Battleship as B

import           Test.Hspec
import           TestHelper      ( fromRight,merge )


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
      (B.shots . B.board2 $ finalGame) `shouldBe` map (\s -> (s,B.Hit)) p1Shots -- Shot results
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
      (B.shots . B.board1 $ finalGame) `shouldBe` map (\s -> (s,B.Hit)) p2Shots -- Shot results
      map fst (B.shots . B.board2 $ finalGame) `shouldBe` p1Shots -- Check at least that the shot coords are recorded.


  describe "boardLargeEnoughForShips" $ do
    it "uses the longest ship and collective ship area to determine the minimum size" $ do
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
      (B.shots . B.board1 $ fromRight $ game) `shouldSatisfy` (not . null)
      (B.shots . B.board2 $ fromRight $ game) `shouldSatisfy` (not . null)
