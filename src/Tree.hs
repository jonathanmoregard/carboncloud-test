module Tree where

import Data.Char
import Data.List

data NodeInfo = NodeInfo
  { cost :: Cost
  , nodeInfoName :: NodeName
  } deriving (Show, Eq)

data Tree
  = Tree_TypeA NodeInfo
               String
               [Tree]
  | Tree_TypeB TypeB

newtype NodeName =
  NodeName String
  deriving (Show, Eq)

newtype Cost =
  Cost Float
  deriving (Show, Eq)

data TypeB =
  TypeB Cost
        NodeName
        [TypeB]

getNodeNames :: Tree -> [NodeName]
getNodeNames (Tree_TypeB b) = getNodeNamesFromB b
getNodeNames (Tree_TypeA i _ []) = [nodeInfoName i]
getNodeNames (Tree_TypeA i _ ns) =
  nodeInfoName i : concat (fmap getNodeNames ns)

getNodeNamesFromB :: TypeB -> [NodeName]
getNodeNamesFromB (TypeB _ n []) = [n]
getNodeNamesFromB (TypeB _ n bs) = n : concat (fmap getNodeNamesFromB bs)

-- Returns unique node names that exists in both trees, that doesn't have the string "bepa" in them
-- Removes duplicates since we consider duplicate names to be instances of the same platonic form
-- In practice, we would have asked the domain experts what the intended functionality is..
getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t t' =
  nub $ filter notBepa $ intersect (getNodeNames t) (getNodeNames t')

notBepa :: NodeName -> Bool
notBepa (NodeName n) = not $ isInfixOf "bepa" $ lower n
  where
    lower = map toLower
