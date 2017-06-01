{-# LANGUAGE TemplateHaskell #-}
module LambdaPunter.Graph where

import Data.Char (toLower)
import Data.Aeson
import Data.Aeson.TH

data Graph = Graph
  { graphNodes :: [Node]
  , graphEdges :: [Edge]
  } deriving (Eq,Show)

newtype Node = Node
  { nodeId :: Int
  } deriving (Eq,Show)

data Edge = Edge
  { edgeSource :: Int
  , edgeTarget :: Int
  } deriving (Eq,Show)

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''Node)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''Edge)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 5} ''Graph)
