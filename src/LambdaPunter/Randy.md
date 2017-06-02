```haskell
module LambdaPunter.Randy where
```

```haskell
import LambdaPunter.Base
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as M
import System.Random (randomRIO)
```

```haskell
randy :: Punter
randy graph punterId game =
  randomElem (available graph game)
```

```haskell
available :: Graph -> Map PunterId [Edge] -> [Edge]
available graph game = graphEdges graph \\ concat (M.elems game)
```

```haskell
randomElem :: [a] -> IO a
randomElem xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
```
