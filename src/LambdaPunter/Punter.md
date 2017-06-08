```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Punter where
```

```haskell
import Data.Map (Map)
import LambdaPunter.Graph
```

Representing punters
---

```haskell
type PunterId = Int

type Punter = Graph -> PunterId -> Map PunterId [Edge] -> IO Edge
```

