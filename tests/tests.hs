module Main where

import Data.List
import Test.Hspec
import Test.QuickCheck
import Tree

main :: IO ()
main =
  hspec $ do
    describe "getCommonNodeNamesExceptBepa does what we want" $ do
      it "returns [] for two Trees with node name 'cepa' but no 'bepa'" $ -- We want this one to fail
       do getCommonNodeNamesExceptBepa cepaButNoBepa cepaButNoBepa `shouldBe` []
      it "Returns all names found in both trees that doesn't contain 'bepa'" $ do
        property $ prop_returnsAllNamesFoundInBothTrees

cepaButNoBepa :: Tree
cepaButNoBepa =
  Tree_TypeA NodeInfo {cost = Cost 0, nodeInfoName = NodeName "cepa"} "" []

prop_returnsAllNamesFoundInBothTrees :: Property
prop_returnsAllNamesFoundInBothTrees =
  forAll
    genNodeNames
    (\nns -> do
       t1 <- genTreeFromNodeNames nns
       t2 <- genTreeFromNodeNames nns
       let distincNames = nub nns
       let combined = getCommonNodeNamesExceptBepa t1 t2
       return $ combined == distincNames)

nodeNamesToGenerate :: Int
nodeNamesToGenerate = 8

genNodeNames :: Gen [NodeName]
genNodeNames = vectorOf nodeNamesToGenerate genNodeName

genNodeName :: Gen NodeName
genNodeName = do
  f <- genChar
  s <- genChar
  return $ NodeName [f, s] -- 4 x 4 = 16 permutations
  where
    genChar = elements ['a' .. 'd']

genTreeFromNodeNames :: [NodeName] -> Gen Tree -- wet, should reuse genTreeWith. Hard since typeB can't have typeA children
genTreeFromNodeNames [] =
  error "genTreeFromNodeNames: Need at least one element" -- test function, edge case is ok
genTreeFromNodeNames (n:[]) = do
  oneof [genTypeANode n [], Tree_TypeB <$> (genTypeBTree [n])]
genTreeFromNodeNames (n:ns) = do
  oneof
    [ (genTypeANode n) =<< (vectorOf 2 $ genTreeFromNodeNames ns)
    , Tree_TypeB <$> (genTypeBTree $ n : ns)
    ]

genTypeANode :: NodeName -> [Tree] -> Gen Tree
genTypeANode n cs = do
  s <- arbitrary
  c <- arbitrary
  return $ Tree_TypeA NodeInfo {cost = Cost c, nodeInfoName = n} s cs

genTypeBTree :: [NodeName] -> Gen TypeB
genTypeBTree = genTreeWith go
  where
    go n cs = do
      c <- arbitrary
      return $ TypeB (Cost c) n cs

genTreeWith :: (NodeName -> [a] -> Gen a) -> [NodeName] -> Gen a
genTreeWith _ [] = error "genTreeWith: Need at least one element" -- test function, edge case is ok
genTreeWith f (n:[]) = f n []
genTreeWith f (n:ns) = (f n) =<< (vectorOf 2 $ genTreeWith f ns)
