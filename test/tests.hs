module Main where

import           Data.Battleship

import           Test.Hspec
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck

main :: IO ()
main = hspec $ do

  it "hello world" $ do
    "hello" `shouldBe` "hello"

  prop "quickcheck hello world" $
    \x -> (read . show) x === (x :: String)


