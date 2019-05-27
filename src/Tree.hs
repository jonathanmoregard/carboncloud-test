module Tree where

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

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa _ _ = []
