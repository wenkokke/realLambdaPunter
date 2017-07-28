```haskell
module LambdaPunter.Randy where
```

```haskell
import LambdaPunter.Base
import qualified Data.Set as S
import qualified Data.IntMap as M
import System.Random (randomRIO)
```

```haskell
randy :: Punter
randy _ _ _ _ legalMoves = randomElem legalMoves
```

```haskell
randomElem :: LegalMoves -> IO River
randomElem xs = (`S.elemAt` xs) <$> randomRIO (0, S.size xs - 1)
-- randomElem xs = (S.toList xs !!) <$> randomRIO (0, S.size xs - 1)
```
