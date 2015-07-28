{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Battleship      as B

import           Test.QuickCheck
import           TestHelper           ( fromRight )


return [] -- What.
main :: IO Bool
main = $quickCheckAll
