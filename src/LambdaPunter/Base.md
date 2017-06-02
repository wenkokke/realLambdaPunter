@realLambdaPunter
===

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Base where
```

```haskell
import Control.Exception
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import LambdaPunter.TH (dropFirstWord)
import System.IO

import Debug.Trace (traceShow)
```

Representing the game graph
---

```haskell
data Graph = Graph
  { graphNodes :: [Node]
  , graphEdges :: [Edge]
  } deriving (Eq,Show)

newtype Node = Node
  { nodeId :: NodeId
  } deriving (Eq,Show)

data Edge = Edge
  { edgeSource :: NodeId
  , edgeTarget :: NodeId
  } deriving (Eq,Show)

type NodeId = Int
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Node)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Edge)
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Graph)
```

Representing punters
---

```haskell
type PunterId = Int

type Punter = Graph -> PunterId -> Map PunterId [Edge] -> IO Edge
```

Representing moves
---

```haskell
data Move = Move
  { movePunter :: PunterId
  , moveSource :: NodeId
  , moveTarget :: NodeId
  }
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Move)
```

Running punters over a handle
---

```haskell
runPunter :: Punter -> Handle -> IO ()
runPunter punter hdl = do
  -- receive punter-id
  msg <- BS.hGetLine hdl
  let
    punterId :: Int
    punterId = decodePunterId (stripLabel "hello punter:" msg)

  -- receive graph
  msg <- BS.hGetLine hdl
  let
    graph :: Graph
    graph = decodeMsg (stripLabel "graph:" msg)

  -- loop
  let
    punterLoop :: Map PunterId [Edge] -> Handle -> IO ()
    punterLoop game hdl = do
      msg <- BS.hGetLine hdl
      putStrLn $ show punterId <> ": " <> BS.unpack msg
      case decodeMove (stripLabel "move:" msg) of
        Just move -> do
          let edge = Edge (moveSource move) (moveTarget move)
          let game' = M.adjust (edge:) (movePunter move) game
          punterLoop game' hdl
        Nothing   -> do
          edge <- punter graph punterId game
          let move = Move punterId (edgeSource edge) (edgeTarget edge)
          let game' = M.adjust (edge:) punterId game
          T.hPutStrLn hdl (encodeToLazyText move)
          punterLoop game' hdl

  Control.Exception.catch
    (punterLoop M.empty hdl)
    (\e -> putStrLn $ show punterId <> ": " <> show (e :: PunterException))
```

```haskell
decodePunterId :: ByteString -> PunterId
decodePunterId = read . BS.unpack

decodeGraph :: ByteString -> Graph
decodeGraph = decodeMsg

decodeMove :: ByteString -> Maybe Move
decodeMove "?" = Nothing
decodeMove msg = traceShow (BS.unpack msg) $ decodeMsg msg
```

```haskell
decodeMsg :: FromJSON a => ByteString -> a
decodeMsg msg =
  case decodeStrict msg of
    Just val -> val
    Nothing  -> throw $ CannotDecode msg
```

```haskell
stripLabel :: String -> ByteString -> ByteString
stripLabel lbl msg =
  case BS.stripPrefix (BS.pack lbl) msg of
    Just val -> val
    Nothing  -> throw $ ExpectedLabel lbl msg
```

```haskell
data PunterException
  = CannotDecode ByteString
  | ExpectedLabel String ByteString
  deriving (Eq,Show)

instance Exception PunterException
```
