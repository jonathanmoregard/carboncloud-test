module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Tree

main :: IO ()
main =
  hspec $ do
    describe "getCommonNodeNamesExceptBepa does what we want" $ do
      it "returns [] for two Trees with node name 'cepa' but no 'bepa'" $ -- We want this one to fail
       do getCommonNodeNamesExceptBepa cepaButNoBepa cepaButNoBepa `shouldBe` []

cepaButNoBepa :: Tree
cepaButNoBepa =
  Tree_TypeA NodeInfo {cost = Cost 0, nodeInfoName = NodeName "cepa"} "" []

genTreeDepth :: Int
genTreeDepth = 4

genTree :: Gen Tree
genTree = go genTreeDepth
  where
    go 0 = do
      oneof [genTypeANode [], Tree_TypeB <$> (genTypeBTree 0)]
    go n = do
      oneof
        [ (listOf $ go (n - 1)) >>= genTypeANode
        , Tree_TypeB <$> (genTypeBTree n)
        ]

genTypeANode :: [Tree] -> Gen Tree
genTypeANode cs = do
  s <- arbitrary
  c <- arbitrary
  n <- genNodeName
  return $ Tree_TypeA NodeInfo {cost = Cost c, nodeInfoName = n} s cs

genTypeBTree :: Int -> Gen TypeB
genTypeBTree 0 = genTypeB []
genTypeBTree n = (listOf $ genTypeBTree (n - 1)) >>= genTypeB

genTypeB :: [TypeB] -> Gen TypeB
genTypeB cs = do
  c <- arbitrary
  n <- genNodeName
  return $ TypeB (Cost c) n cs

genNodeName :: Gen NodeName
genNodeName = do
  f <- genChar
  s <- genChar
  return $ NodeName [f, s] -- 4 x 4 = 16 permutations
  where
    genChar = elements ['a' .. 'd']
