```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Graph where
```

```haskell
import Data.Aeson.TH
import LambdaPunter.TH (dropFirstWord)
```

Representing the game graph
---

```haskell
data Graph = Graph
  { graphNodes :: [Node]
  , graphEdges :: [Edge]
  } deriving (Eq,Show)

newtype Node = Node
  { nodeId :: NodeId
  } deriving (Eq)

data Edge = Edge
  { edgeSource :: NodeId
  , edgeTarget :: NodeId
  }

type NodeId = Int
```

```haskell
instance Eq Edge where
  (Edge x1 y1) == (Edge x2 y2) =
    (x1 == x2 && y1 == y2) ||
    (y1 == x2 && x1 == y2)
```

```haskell
instance Show Node where
  show (Node id) = show id

instance Show Edge where
  show (Edge x y) = show (x,y)
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Node)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Edge)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Graph)
```
