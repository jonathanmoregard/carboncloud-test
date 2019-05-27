module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Tree

main :: IO ()
main =
  hspec $ do
    describe "getCommonNodeNamesExceptBepa does what we want" $ do
      it "returns [] for two Trees with node name 'cepa' but no 'bepa'" $ do
        getCommonNodeNamesExceptBepa cepaButNoBepa cepaButNoBepa `shouldBe` []

cepaButNoBepa :: Tree
cepaButNoBepa =
  Tree_TypeA NodeInfo {cost = Cost 0, nodeInfoName = NodeName "cepa"} "" []
