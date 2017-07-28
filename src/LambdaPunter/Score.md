```haskell
{-# LANGUAGE RecordWildCards #-}
module LambdaPunter.Score where
```

```haskell
import Data.Coerce (coerce)
import qualified Data.Graph.Inductive.PatriciaTree as F (Gr)
import qualified Data.Graph.Inductive.Graph as F
import qualified Data.Graph.Inductive.Query.DFS as F (reachable)
import qualified Data.Graph.Inductive.Query.SP as F
import qualified Data.IntMap as M
import Data.Maybe (catMaybes)
import LambdaPunter.Map
```

```haskell
type SiteMap a = M.IntMap a
type ScoringData = SiteMap (SiteMap Int)
```

```haskell
mkScoringData :: Map -> ScoringData
mkScoringData Map{..} = M.fromList
  [(mineId, treeToMap $ F.spTree mineId gameGr) | mineId <- graphMines]
  where
    gameGr :: Real b => F.Gr () b
    gameGr = F.mkGraph lsites lrivers
      where
        lsites :: [F.LNode ()]
        lsites = [(siteId site, ()) | site <- graphSites]
        lrivers :: Real b => [F.LEdge b]
        lrivers = [(riverSource river, riverTarget river, 1) | river <- graphRivers]
               ++[(riverTarget river, riverSource river, 1) | river <- graphRivers]

    treeToMap :: Real b => F.LRTree b -> SiteMap b
    treeToMap paths = M.fromList
      [(siteId, weightSum ^ 2)
      |path <- paths, let (siteId:_, weightSum:_) = unzip (coerce path)]
```

```haskell
score :: Map -> ScoringData -> [River] -> Int
score Map{..} scoringData punterRivers = sum . catMaybes $
  [M.lookup siteId =<< M.lookup mineId scoringData
  |mineId <- graphMines, siteId <- reachable M.! mineId, siteId /= mineId]
  where
    punterGr :: F.Gr () ()
    punterGr = F.mkGraph lsites lrivers
      where
        lsites :: [F.LNode ()]
        lsites = [(siteId site, ()) | site <- graphSites]
        lrivers :: [F.LEdge ()]
        lrivers = [(riverSource river, riverTarget river, ()) | river <- punterRivers]
               ++[(riverTarget river, riverSource river, ()) | river <- punterRivers]

    reachable :: SiteMap [SiteId]
    reachable = M.fromList
      [(mineId, F.reachable mineId punterGr) | mineId <- graphMines]
```
