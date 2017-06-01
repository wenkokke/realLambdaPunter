module LambdaPunter.Player where

import Data.Map (Map)
import LambdaPunter.Graph

type PlayerId = Int
type Player   = Graph -> Map PlayerId [Edge] -> Edge

