{-# LANGUAGE RecordWildCards #-}
module LambdaPunter.Score where

import Data.Coerce (coerce)
import qualified Data.Graph.Inductive.PatriciaTree as F (Gr)
import qualified Data.Graph.Inductive.Graph as F
import qualified Data.Graph.Inductive.Query.DFS as F (reachable)
import qualified Data.Graph.Inductive.Query.SP as F
import qualified Data.IntMap as M
import Data.Maybe (catMaybes)
import LambdaPunter.Graph

type NodeMap a = M.IntMap a

-- 1. compute reachable components within the punter's subgraph from each mine
-- 2. compute shortest paths from each mine within the supergraph

score :: Graph -> [Edge] -> Int
score Graph{..} punterEdges = sum . catMaybes $
  [M.lookup nodeId (scores mineId)
  | mineId <- graphMines, nodeId <- reachable M.! mineId]
  where
    lnodes :: [F.LNode ()]
    lnodes = map (\n -> (nodeId n , ())) graphNodes

    gameGr :: F.Gr () Int
    gameGr = F.mkGraph lnodes ledges
      where
        ledges :: [F.LEdge Int]
        ledges = map (\e -> (edgeSource e, edgeTarget e, 1)) graphEdges

    punterGr :: F.Gr () ()
    punterGr = F.mkGraph lnodes ledges
      where
        ledges :: [F.LEdge ()]
        ledges = map (\e -> (edgeSource e, edgeTarget e, ())) punterEdges

    reachable :: NodeMap [NodeId]
    reachable = M.fromList $ map (\nodeId -> (nodeId, F.reachable nodeId punterGr)) graphMines

    scores :: NodeId -> NodeMap Int
    scores nodeId = treeToMap $ F.spTree nodeId gameGr
      where
        treeToMap :: F.LRTree Int -> NodeMap Int
        treeToMap paths = M.fromList
          [(last nodeIds, weightsToScore weights)
          | path <- paths, let (nodeIds, weights) = unzip (coerce path)]

    weightsToScore :: [Int] -> Int
    weightsToScore weights = (sum weights) ^ 2
