```haskell
module LambdaPunter.Randy where
```

```haskell
import LambdaPunter.Base
import Data.List ((\\))
import qualified Data.IntMap as M
import System.Random (randomRIO)
```

```haskell
randy :: Punter
randy graph punterId game = randomElem $ available graph game
```

```haskell
available :: Graph -> Game -> [Edge]
available graph game = graphEdges graph \\ concat (M.elems game)
```

```haskell
randomElem :: [a] -> IO a
randomElem xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
```
