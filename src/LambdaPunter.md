> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE OverloadedStrings #-}
> module LambdaPunter where

> import Data.Aeson
> import Data.Aeson.TH
> import Data.ByteString (ByteString)
> import qualified Data.ByteString as BS
> import qualified Data.ByteString.UTF8 as BS
> import Data.Map (Map)
> import qualified Data.Map as M
> import Data.Monoid ((<>))
> import LambdaPunter.TH (dropFirstWord)
> import System.IO

Graphs
------

> data Graph = Graph
>   { graphNodes :: [Node]
>   , graphEdges :: [Edge]
>   } deriving (Eq,Show)

> newtype Node = Node
>   { nodeId :: NodeId
>   } deriving (Eq,Show)

> data Edge = Edge
>   { edgeSource :: NodeId
>   , edgeTarget :: NodeId
>   } deriving (Eq,Show)

> type NodeId = Int

> $(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Node)
> $(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Edge)
> $(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Graph)

Punters
-------

Definition
==========

> type PunterId = Int

> type Punter = Graph -> PunterId -> Map PunterId [Edge] -> Edge

Moves
=====

> data Move = Move
>   { movePunter :: PunterId
>   , moveSource :: NodeId
>   , moveTarget :: NodeId
>   }

> $(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Move)

Running punters
===============

> runPunter :: Punter -> Handle -> IO ()
> runPunter punter hdl = do
>   -- receive punter-id
>   msg <- BS.hGetLine hdl
>   let
>     punterId :: Int
>     punterId = decodePunterId (stripLabel "hello punter:" msg)
>
>   -- receive graph
>   msg <- BS.hGetLine hdl
>   let
>     graph :: Graph
>     graph = decodeMsg (stripLabel "graph:" msg)
>
>   -- loop
>   let
>     punterLoop :: Map PunterId [Edge] -> Handle -> IO ()
>     punterLoop game hdl = do
>       msg <- BS.hGetLine hdl
>       case decodeMove msg of
>         Just move -> do
>           let edge  = Edge (moveSource move) (moveTarge
>           let game' = M.adjust (edge:) (movePunter move) game
>           punterLoop game' hdl
>         Nothing   -> do
>           let edge  = punter graph punterId game
>           let move  = Move punterId (edgeSource edge) (edgeTarget edge)
>           BSL.hPutStr hdl encode
>           punterLoop game' hdl
>
>   punterLoop M.empty hdl

> decodePunterId :: ByteString -> PunterId
> decodePunterId = read . BS.toString

> decodeGraph :: ByteString -> Graph
> decodeGraph = decodeMsg

> decodeMove :: ByteString -> Maybe Move
> decodeMove "?" = Nothing
> decodeMove msg = decodeMsg msg

> decodeMsg :: FromJSON a => ByteString -> a
> decodeMsg msg =
>   case decodeStrict msg of
>     Just val -> val
>     Nothing  -> error $ "cannot decode message \"" <> BS.toString msg <> "\""

> stripLabel :: String -> ByteString -> ByteString
> stripLabel lbl msg =
>   case BS.stripPrefix (BS.fromString lbl) msg of
>     Just val -> val
>     Nothing  -> error $ "expected message with label \"" <> lbl <> "\""
