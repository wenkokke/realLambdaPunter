```haskell
{-# LANGUAGE RecordWildCards #-}
module LambdaPunter.Tortoise where
```

```haskell
import Control.Monad (when)
import Data.Function (on)
import qualified Data.IntMap as M
import Data.IORef
import Data.List (find,maximumBy)
import Data.Maybe (fromMaybe,fromJust)
import Data.Tree (Tree,Forest)
import qualified Data.Tree as T
import Debug.Trace (traceShow)
import LambdaPunter.Base
import LambdaPunter.Randy (available)
```

```haskell
-- assume: no punter will ever pass
mkGameTree :: Graph -> PunterId -> Game -> Forest (PunterId, Edge)
mkGameTree graph myId game = go game (turnCycle 0)
  where
    maxEdges = length $ graphEdges graph
    numPunters = maximum $ M.keys game

    -- assume: the punters are always given turns in order 0..numPunters
    turnCycle :: PunterId -> [PunterId]
    turnCycle punterId = punterId : rest
      where
        rest | punterId >= numPunters = turnCycle 0
             | otherwise              = turnCycle (punterId + 1)

    go :: Game -> [PunterId] -> Forest (PunterId, Edge)
    go game (punterId:rest) = do
      edge <- available graph game
      let game' = claimEdge punterId edge game
      return . T.Node (punterId, edge) $
        if numEdges game' >= maxEdges then [] else go game' rest
```

```haskell
type ScoreMap = M.IntMap Int

optimal :: Graph -> ScoringData -> PunterId -> Game
        -> Forest (PunterId, Edge) -> Forest (PunterId, Edge, Int)
optimal graph scoringData myId game = map (fst . go game)
  where
    go :: Game -> Tree (PunterId, Edge) -> (Tree (PunterId, Edge, Int), ScoreMap)
    go game (T.Node (punterId, edge) []) = let

      -- assume: each punter has an entry in the graph
      numPunters = maximum $ M.keys game

      -- compute the point-value for each punter in this final state
      scores = mkScoreMap graph scoringData game

      in (T.Node (punterId, edge, scores M.! myId) [], scores)

    -- if there are still moves left to make, we pick the move which
    -- maximizes the point-value of the punter whose turn it is
    go game (T.Node (punterId, edge) subForest) = let

      -- make the recursive call
      goForest = map (go (claimEdge punterId edge game))

      -- get the current punter's score
      currentPunterScore = (M.! punterId) . snd

      in maximumBy (compare `on` currentPunterScore) (goForest subForest)
```

```haskell
-- assume: a punter wants to maximise the difference between their score and
--         the best other score, either taking as large a lead as possible,
---        or getting as close as possible to the first-placed punter
mkScoreMap :: Graph -> ScoringData -> Game -> ScoreMap
mkScoreMap graph scoringData game = punterLead
  where
    -- assume: each punter has an entry in the graph
    numPunters = length $ M.keys game

    scores = M.fromList
      [(punterId, score graph scoringData (game M.! punterId))
      |punterId <- [0..numPunters - 1]]

    punterLead = M.fromList
      [(punterId, myScore - bestOtherScore)
      |punterId <- [0..numPunters - 1]
      ,let myScore = scores M.! punterId
      ,let bestOtherScore = maximum (M.elems (M.delete punterId scores))]
```

```haskell
tortoise :: IORef (Maybe (Forest (PunterId, Edge, Int))) -> Punter
tortoise ioRef graph scoringData myId = go
  where
    go :: Game -> IO Edge
    go game = do
      -- we either:
      --   a) read the tree from memory; or
      --   b) create the tree on the fly
      let defGameTree = mkGameTree graph myId game
      let defOptimalTree = optimal graph scoringData myId game defGameTree
      gameTree <- fromMaybe defOptimalTree <$> readIORef ioRef
      -- we walk down the tree:
      --   * if a level is selecting another punter's move we check the game state
      --     and use that move;
      --   * otherwise, we select the optimal move and return the updated tree
      let T.Node (_,bestMove,_) gameTree' = applyMoves myId game gameTree
      -- we then write the updated tree to memory so we can reuse it next iteration
      writeIORef ioRef (Just gameTree')
      return bestMove
```

Walk down the game tree selecting the moves each opposing punter selected.

```haskell
applyMoves :: PunterId -> Game -> Forest (PunterId, Edge, Int) -> Tree (PunterId, Edge, Int)
applyMoves myId game gameTree
  | punterId == myId = maximumBy (compare `on` nodeToScore) gameTree
  | otherwise        = applyMoves myId game gameTree'
  where
    -- assume: all nodes in a forest refer to moves by the same punter
    punterId   = nodeToPunterId (head gameTree)
    -- assume: the game state is always up to date, and conses on the latest move
    latestMove = head (game M.! punterId)
    gameTree'  = T.subForest (fromJust (find ((==latestMove) . nodeToEdge) gameTree))
```

```haskell
nodeToPunterId :: Tree (PunterId, Edge, Int) -> PunterId
nodeToPunterId (T.Node (punterId, _, _) _) = punterId

nodeToEdge :: Tree (PunterId, Edge, Int) -> Edge
nodeToEdge (T.Node (_, edge, _) _) = edge

nodeToScore :: Tree (PunterId, Edge, Int) -> Int
nodeToScore (T.Node (_, _, score) _) = score

numEdges :: Game -> Int
numEdges = sum . map length . M.elems
```
