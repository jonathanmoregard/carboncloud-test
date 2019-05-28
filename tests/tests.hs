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
      it "Does not return any name containing 'bepa' no matter the case" $ do
        property $ prop_filtersAllBepaNames
      it "Lets non-bepa names through even though all bepa names are filtered" $ do
        property $ prop_letsNamesThroughSelectively

cepaButNoBepa :: Tree
cepaButNoBepa =
  Tree_TypeA NodeInfo {cost = Cost 0, nodeInfoName = NodeName "cepa"} "" []

prop_returnsAllNamesFoundInBothTrees :: Property
prop_returnsAllNamesFoundInBothTrees =
  forAll
    (vectorOf 8 genNodeName)
    (\nns -> do
       t1 <- genTreeFromNodeNames nns
       t2 <- genTreeFromNodeNames nns
       let distincNames = nub nns
       let combined = getCommonNodeNamesExceptBepa t1 t2
       return $ combined == distincNames)

prop_filtersAllBepaNames :: Property
prop_filtersAllBepaNames =
  forAll
    (vectorOf 8 genSneakyBepaNodeName)
    (\bns -> do
       t1 <- genTreeFromNodeNames bns
       t2 <- genTreeFromNodeNames bns
       let combined = getCommonNodeNamesExceptBepa t1 t2
       return $ combined == [])

prop_letsNamesThroughSelectively :: Property
prop_letsNamesThroughSelectively =
  forAll
    (vectorOf 5 genNodeName)
    (\nns -> do
       bns <- (vectorOf 3 genSneakyBepaNodeName)
       t1 <- genTreeFromNodeNames $ nns ++ bns
       t2 <- genTreeFromNodeNames $ bns ++ nns
       let distincNames = nub nns
       let combined = getCommonNodeNamesExceptBepa t1 t2
       return $ combined == distincNames)

genNodeName :: Gen NodeName
genNodeName = do
  f <- genChar
  s <- genChar
  return $ NodeName [f, s] -- 4 x 4 = 16 permutations
  where
    genChar = elements ['a' .. 'd']

genSneakyBepaNodeName :: Gen NodeName
genSneakyBepaNodeName = do
  prefix <- arbitrary
  b <- elements ['b', 'B']
  e <- elements ['e', 'E']
  p <- elements ['p', 'P']
  a <- elements ['a', 'A']
  postfix <- arbitrary
  return $ NodeName $ prefix ++ [b, e, p, a] ++ postfix

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
