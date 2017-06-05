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
  } deriving (Eq)

data Edge = Edge
  { edgeSource :: NodeId
  , edgeTarget :: NodeId
  }

type NodeId = Int
```

```haskell
instance Eq Edge where
  (Edge x1 y1) == (Edge x2 y2) =
    (x1 == x2 && y1 == y2) ||
    (y1 == x2 && x1 == y2)
```

```haskell
instance Show Node where
  show (Node id) = show id

instance Show Edge where
  show (Edge x y) = show (x,y)
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
data Msg
  = Query
  | Info Move
  | End

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
    punterId = decodePunterId msg

  -- receive graph
  msg <- BS.hGetLine hdl
  let
    graph :: Graph
    graph = decodeGraph msg

  -- loop
  let
    punterLoop :: Map PunterId [Edge] -> Handle -> IO ()
    punterLoop game hdl = do
      msg <- BS.hGetLine hdl
      case decodeMsg msg of
        Query     -> do
          edge <- punter graph punterId game
          let move = Move punterId (edgeSource edge) (edgeTarget edge)
          let game' = claimEdge punterId edge game
          T.hPutStrLn hdl (encodeToLazyText move)
          punterLoop game' hdl
        Info move -> do
          let edge = Edge (moveSource move) (moveTarget move)
          let game' = claimEdge (movePunter move) edge game
          punterLoop game' hdl
        End       -> putStrLn "Done"

  Control.Exception.catch
    (punterLoop M.empty hdl)
    (\e -> putStrLn $ show punterId <> ": " <> show (e :: PunterException))
```

```haskell
claimEdge :: PunterId -> Edge -> Map PunterId [Edge] -> Map PunterId [Edge]
claimEdge punterId edge game
  | M.member punterId game = M.adjust (edge:) punterId game
  | otherwise              = M.insert punterId [edge] game
```

```haskell
decodePunterId :: ByteString -> PunterId
decodePunterId = read . BS.unpack . stripLabel "hello punter:"

decodeGraph :: ByteString -> Graph
decodeGraph = decodeJSON . stripLabel "graph:"

decodeMsg :: ByteString -> Msg
decodeMsg "end"    = End
decodeMsg "move:?" = Query
decodeMsg msg      = Info (decodeJSON (stripLabel "move:" msg))
```

```haskell
decodeJSON :: FromJSON a => ByteString -> a
decodeJSON msg =
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
