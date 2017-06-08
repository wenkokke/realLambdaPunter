```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Move where
```

```haskell
import Data.Aeson.TH
import LambdaPunter.TH (dropFirstWord)
import LambdaPunter.Graph
import LambdaPunter.Punter
```

Representing moves
---

```haskell
data Move = Move
  { movePunter :: PunterId
  , moveSource :: NodeId
  , moveTarget :: NodeId
  }
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Move)
```
