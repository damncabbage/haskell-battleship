module Main where

import qualified Data.Battleship as B

import           Test.Hspec
import           TestHelper      ( fromRight,merge )


main :: IO ()
main = hspec $ do

  it "hello world" $ do
    "hello" `shouldBe` "hello"
