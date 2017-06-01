{-# LANGUAGE TemplateHaskell #-}
module LambdaPunter.Graph where

import Data.Char (toLower)
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.UTF8 as BS
import Data.Monoid ((<>))
import System.IO

newtype Player =
  Player { run :: Handle -> IO () }


-- |Graph type used by Lambda Punter
data Graph = Graph { graphNodes :: [Node], graphEdges :: [Edge] }
  deriving (Eq,Show)

newtype Node = Node { nodeId :: Int }
  deriving (Eq,Show)

data Edge = Edge { edgeSource :: Int, edgeTarget :: Int }
  deriving (Eq,Show)

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''Node)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''Edge)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 5} ''Graph)

-- |Example graph
exampleGraph :: Graph
exampleGraph =
  Graph
  { nodes = map Node [0..7]
  , edges =
    [Edge { source = 4, target = 3 }
    ,Edge { source = 1, target = 0 }
    ,Edge { source = 3, target = 2 }
    ,Edge { source = 3, target = 1 }
    ,Edge { source = 6, target = 5 }
    ,Edge { source = 5, target = 4 }
    ,Edge { source = 5, target = 3 }
    ,Edge { source = 7, target = 6 }
    ,Edge { source = 7, target = 5 }
    ,Edge { source = 7, target = 1 }
    ,Edge { source = 7, target = 0 }
    ,Edge { source = 2, target = 1 }
    ]
  }

somePlayer :: Player
somePlayer = Player $ \hdl -> do
  -- receive a player-id
  msg <- BS.hGetLine hdl
  let
    playerId :: Int
    playerId = read . BS.toString $ stripLabel "hello punter:" msg
  putStrLn $ "player-id: " ++ show playerId

  -- receive the graph
  msg <- BS.hGetLine hdl
  let
    graph :: Maybe Graph
    graph = decodeStrict $ stripLabel "graph:" msg
  print graph


stripLabel :: String -> ByteString -> ByteString
stripLabel lbl msg =
  case BS.stripPrefix (BS.fromString lbl) msg of
    Just val -> val
    Nothing  -> error $ "expected message with label \"" <> lbl <> "\""
