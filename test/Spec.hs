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


