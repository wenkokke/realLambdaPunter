```haskell
module LambdaPunter.Greedo where
```

```haskell
import Data.Function (on)
import qualified Data.IntMap as M
import Data.List (maximumBy)
import LambdaPunter.Base
import LambdaPunter.Randy (available)
import LambdaPunter.Tortoise (mkScoreMap)
```

```haskell
greedo :: Punter
greedo graph scoringData myId = go
  where
    go :: Game -> IO Edge
    go game = return . fst . maximumBy (compare `on` snd) $ do
      edge <- available graph game
      let scores  = mkScoreMap graph scoringData (claimEdge myId edge game)
      let myScore = scores M.! myId
      return (edge, myScore)
```
