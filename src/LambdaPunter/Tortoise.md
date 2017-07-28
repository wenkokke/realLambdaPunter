```haskell
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
import qualified Data.Set as S
import Data.Tree (Tree,Forest)
import qualified Data.Tree as T
import LambdaPunter.Base
```

We build up a game tree, containing each possible move for each punter. This
assumes that no punter will ever choose to "pass", because if we allow passing
the tree becomes infinite and also why would you *ever* pass?

```haskell
mkGameTree :: Map -> PunterId -> Game -> LegalMoves -> Forest (PunterId, River)
mkGameTree graph myId game legalMoves = go game (turnCycle 0)
  where
    numRivers = sum . map length . M.elems
    maxRivers = length $ graphRivers graph
    numPunters = maximum $ M.keys game

    -- assume: the punters are always given turns in order 0..numPunters
    turnCycle :: PunterId -> [PunterId]
    turnCycle punterId = punterId : rest
      where
        rest | punterId >= numPunters = turnCycle 0
             | otherwise              = turnCycle (punterId + 1)

    -- assume: no punter will ever pass
    go :: Game -> [PunterId] -> Forest (PunterId, River)
    go game (punterId:rest) = do
      river <- S.toList legalMoves
      let game' = claimRiver punterId river game
      return . T.Node (punterId, river) $
        if numRivers game' >= maxRivers then [] else go game' rest
```

We then augment this tree with scores, computing the scores for each punter at
each possible outcome. We then propagate these scores down the tree by, at every
move, choosing the alternative which maximizes the score of the punter whose
turn it is. We store the score of the punter we're running at each site.

```haskell
type ScoreMap = M.IntMap Int

optimal :: Map -> ScoringData -> PunterId -> Game
        -> Forest (PunterId, River) -> Forest (PunterId, River, Int)
optimal graph scoringData myId game = map (fst . go game)
  where
    go :: Game -> Tree (PunterId, River) -> (Tree (PunterId, River, Int), ScoreMap)
    go game (T.Node (punterId, river) []) = let

      -- assume: each punter has an entry in the graph
      numPunters = maximum $ M.keys game

      -- compute the point-value for each punter in this final state
      scores = mkScoreMap graph scoringData game

      in (T.Node (punterId, river, scores M.! myId) [], scores)

    -- if there are still moves left to make, we pick the move which
    -- maximizes the point-value of the punter whose turn it is
    go game (T.Node (punterId, river) subForest) = let

      -- make the recursive call
      goForest = map (go (claimRiver punterId river game))

      -- get the current punter's score
      currentPunterScore = (M.! punterId) . snd

      in maximumBy (compare `on` currentPunterScore) (goForest subForest)
```

We compute a score map as described above: each punter wishes to maximize their
lead over the other punters, or at very least come as close as possible to the
first-placed punter.

```haskell
mkScoreMap :: Map -> ScoringData -> Game -> ScoreMap
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
tortoise :: IORef (Maybe (Forest (PunterId, River, Int))) -> Punter
tortoise ioRef graph scoringData myId = go
  where
    go :: Game -> LegalMoves -> IO River
    go game legalMoves = do
      let defGameTree = mkGameTree graph myId game legalMoves
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
applyMoves :: PunterId -> Game -> Forest (PunterId, River, Int) -> Tree (PunterId, River, Int)
applyMoves myId game gameTree
  | punterId == myId = maximumBy (compare `on` siteToScore) gameTree
  | otherwise        = applyMoves myId game gameTree'
  where
    -- assume: all sites in a forest refer to moves by the same punter
    punterId   = siteToPunterId (head gameTree)
    -- assume: the game state is always up to date, and conses on the latest move
    latestMove = head (game M.! punterId)
    gameTree'  = T.subForest (fromJust (find ((==latestMove) . siteToRiver) gameTree))
```

Last, we've got a bunch of helper functions to deal with these tree sites and
such like.

```haskell
siteToPunterId :: Tree (PunterId, River, Int) -> PunterId
siteToPunterId (T.Node (punterId, _, _) _) = punterId

siteToRiver :: Tree (PunterId, River, Int) -> River
siteToRiver (T.Node (_, river, _) _) = river

siteToScore :: Tree (PunterId, River, Int) -> Int
siteToScore (T.Node (_, _, score) _) = score
```
