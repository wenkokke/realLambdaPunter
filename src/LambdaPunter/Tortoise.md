```haskell
{-# LANGUAGE RecordWildCards #-}
module LambdaPunter.Tortoise where
```

Tortoise is a punter AI which will compute the theoretically optimal move. It
assumes that each punter wants to maximize their lead over the second-best
punter, or minimize their distance to first place.

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

We build up a game tree, containing each possible move for each punter. This
assumes that no punter will ever choose to "pass", because if we allow passing
the tree becomes infinite and also why would you *ever* pass?

```haskell
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

    -- assume: no punter will ever pass
    go :: Game -> [PunterId] -> Forest (PunterId, Edge)
    go game (punterId:rest) = do
      edge <- available graph game
      let game' = claimEdge punterId edge game
      return . T.Node (punterId, edge) $
        if numEdges game' >= maxEdges then [] else go game' rest
```

We then augment this tree with scores, computing the scores for each punter at
each possible outcome. We then propagate these scores down the tree by, at every
move, choosing the alternative which maximizes the score of the punter whose
turn it is. We store the score of the punter we're running at each node.

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

We compute a score map as described above: each punter wishes to maximize their
lead over the other punters, or at very least come as close as possible to the
first-placed punter.

```haskell
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

Tortoise is slow enough if we only compute the game tree once, therefore we
request some memory, and store it there. Given memory, we either:

  - read the tree from memory; or
  - create the tree on the fly.

After each move, we walk down the tree:

  - if a level is selecting another punter's move we check the game state
    and use that move;
  - otherwise, we select the optimal move and return the updated tree.
  
We then write the updated tree to memory so we can reuse it on the next
iteration.

```haskell
tortoise :: IORef (Maybe (Forest (PunterId, Edge, Int))) -> Punter
tortoise ioRef graph scoringData myId = go
  where
    go :: Game -> IO Edge
    go game = do
      let defGameTree = mkGameTree graph myId game
      let defOptimalTree = optimal graph scoringData myId game defGameTree
      gameTree <- fromMaybe defOptimalTree <$> readIORef ioRef
      let T.Node (_,bestMove,_) gameTree' = applyMoves myId game gameTree
      writeIORef ioRef (Just gameTree')
      return bestMove
```

The function `applyMoves` walks down the game tree, selecting the moves each
opposing punter actually selected at each step, until we reach the level for the
current punter's move. Once there, we select the move which maximizes the score.

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

Last, we've got a bunch of helper functions to deal with these tree nodes and
such.

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
