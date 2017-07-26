```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Graph where
```

```haskell
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import LambdaPunter.TH (dropFirstWord)
```

Representing the game graph
---

```haskell
newtype GraphWrapper = GraphWrapper
  { wrappedGraph :: Graph
  } deriving (Eq,Show)

data Graph = Graph
  { graphNodes :: [Node]
  , graphEdges :: [Edge]
  , graphMines :: [NodeId]
  } deriving (Eq,Show)

newtype Node = Node
  { nodeId :: NodeId
  } deriving (Eq,Ord,Generic)

data Edge = Edge
  { edgeSource :: NodeId
  , edgeTarget :: NodeId
  } deriving (Ord,Generic)

type NodeId = Int
```

```haskell
instance Eq Edge where
  e1 == e2 =
    (edgeSource e1 == edgeSource e2 && edgeTarget e1 == edgeTarget e2) ||
    (edgeSource e1 == edgeTarget e2 && edgeTarget e1 == edgeSource e2)
```

```haskell
instance Hashable Node
instance Hashable Edge
```

```haskell
instance Show Node where
  show Node{..} = show nodeId

instance Show Edge where
  show Edge{..} = show (edgeSource,edgeTarget)
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Node)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Edge)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Graph)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''GraphWrapper)
```
