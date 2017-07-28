```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Map where
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
newtype MapWrapper = MapWrapper
  { wrappedMap :: Map
  } deriving (Eq,Show)

data Map = Map
  { graphSites :: [Site]
  , graphRivers :: [River]
  , graphMines :: [SiteId]
  } deriving (Eq,Show)

newtype Site = Site
  { siteId :: SiteId
  } deriving (Eq,Ord,Generic)

data River = River
  { riverSource :: SiteId
  , riverTarget :: SiteId
  } deriving (Ord,Generic)

type SiteId = Int
```

```haskell
instance Eq River where
  e1 == e2 =
    (riverSource e1 == riverSource e2 && riverTarget e1 == riverTarget e2) ||
    (riverSource e1 == riverTarget e2 && riverTarget e1 == riverSource e2)
```

```haskell
instance Hashable Site
instance Hashable River
```

```haskell
instance Show Site where
  show Site{..} = show siteId

instance Show River where
  show River{..} = show (riverSource,riverTarget)
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Site)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''River)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Map)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''MapWrapper)
```
