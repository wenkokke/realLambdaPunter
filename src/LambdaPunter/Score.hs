{-# LANGUAGE RecordWildCards #-}
module LambdaPunter.Score where

import Data.Coerce (coerce)
import qualified Data.Graph.Inductive.PatriciaTree as F (Gr)
import qualified Data.Graph.Inductive.Graph as F
import qualified Data.Graph.Inductive.Query.DFS as F (reachable)
import qualified Data.Graph.Inductive.Query.SP as F
import qualified Data.IntMap as M
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow)
import LambdaPunter.Graph

type NodeMap a = M.IntMap a

-- 1. compute reachable components within the punter's subgraph from each mine
-- 2. compute shortest paths from each mine within the supergraph
-- TODO the graph is _undirected_ whereas the code below treats it as directed

type ScoringData = NodeMap (NodeMap Int)

mkScoringData :: Graph -> ScoringData
mkScoringData Graph{..} = M.fromList
  [(mineId, treeToMap $ F.spTree mineId gameGr) | mineId <- graphMines]
  where
    gameGr :: Real b => F.Gr () b
    gameGr = F.mkGraph lnodes ledges
      where
        lnodes :: [F.LNode ()]
        lnodes = [(nodeId node, ()) | node <- graphNodes]
        ledges :: Real b => [F.LEdge b]
        ledges = [(edgeSource edge, edgeTarget edge, 1) | edge <- graphEdges]
               ++[(edgeTarget edge, edgeSource edge, 1) | edge <- graphEdges] 

    treeToMap :: Real b => F.LRTree b -> NodeMap b
    treeToMap paths = M.fromList
      [(nodeId, weightSum ^ 2)
      |path <- paths, let (nodeId:_, weightSum:_) = unzip (coerce path)]


score :: Graph -> ScoringData -> [Edge] -> Int
score Graph{..} scoringData punterEdges = sum . catMaybes $
  [M.lookup nodeId =<< M.lookup mineId scoringData
  |mineId <- graphMines, nodeId <- reachable M.! mineId, nodeId /= mineId]
  where
    punterGr :: F.Gr () ()
    punterGr = F.mkGraph lnodes ledges
      where
        lnodes :: [F.LNode ()]
        lnodes = [(nodeId node, ()) | node <- graphNodes]
        ledges :: [F.LEdge ()]
        ledges = [(edgeSource edge, edgeTarget edge, ()) | edge <- punterEdges]
               ++[(edgeTarget edge, edgeSource edge, ()) | edge <- punterEdges]

    reachable :: NodeMap [NodeId]
    reachable = M.fromList
      [(mineId, F.reachable mineId punterGr) | mineId <- graphMines]
