```haskell
module LambdaPunter.Greedo where
```

```haskell
import Data.Function (on)
import qualified Data.IntMap as M
import Data.List (maximumBy)
import qualified Data.Set as S
import LambdaPunter.Base
import LambdaPunter.Tortoise (mkScoreMap)
```

```haskell
greedo :: Punter
greedo graph scoringData myId = go
  where
    go :: Game -> LegalMoves -> IO Edge
    go game legalMoves = return . fst . maximumBy (compare `on` snd) $ do
      edge <- S.toList legalMoves
      let scores  = mkScoreMap graph scoringData (claimEdge myId edge game)
      let myScore = scores M.! myId
      return (edge, myScore)
```
