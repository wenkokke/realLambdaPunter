@realLambdaPunter
===

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Base
  (Game
  ,PunterId
  ,Punter
  ,Move(..)
  ,Msg(..)
  ,PunterException(..)
  ,runPunter
  ,claimEdge
  ,module X
  ) where
```

```haskell
import Control.Exception
import Control.Monad (forM_)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as M
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import LambdaPunter.TH (dropFirstWord)
import LambdaPunter.Graph as X
import LambdaPunter.Score as X
import System.IO
```

```haskell
import qualified Data.IntMap as M
import LambdaPunter.Graph
```

Representing the game state
---

```haskell
type Game = M.IntMap [Edge]
```

Representing punters
---

```haskell
type PunterId = Int
```

```haskell
type Punter = Graph -> ScoringData -> PunterId -> Game -> IO Edge
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

Representing messages
---

```haskell
data Msg
  = Query
  | Info Move
  | End Int
```

Running punters over a handle
---

```haskell
runPunter :: Punter -> Handle -> IO ()
runPunter punter hdl = do
  -- receive punter-id
  msg <- BS.hGetLine hdl
  let
    (punterId, numPunters) = decodePunterInfo msg

  -- receive graph
  msg <- BS.hGetLine hdl
  let
    graph :: Graph
    graph = decodeGraph msg
    scoringData :: ScoringData
    scoringData = mkScoringData graph
    realPunter :: Game -> IO Edge
    realPunter = punter graph scoringData punterId
    initGame :: Game
    initGame = foldr (\punterId -> M.insert punterId []) M.empty [0..numPunters]

  -- loop
  let
    punterLoop :: Game -> Handle -> IO ()
    punterLoop game hdl = do
      msg <- BS.hGetLine hdl
      case decodeMsg msg of
        Query     -> do
          edge <- realPunter game
          let move = Move punterId (edgeSource edge) (edgeTarget edge)
          let game' = claimEdge punterId edge game
          T.hPutStrLn hdl (encodeToLazyText move)
          punterLoop game' hdl
        Info move -> do
          let edge = Edge (moveSource move) (moveTarget move)
          let game' = claimEdge (movePunter move) edge game
          punterLoop game' hdl
        End scoreRemote -> do
          let scoreLocal = score graph scoringData (game M.! punterId)
          assert (scoreRemote == scoreLocal) $
            putStrLn $
              "Punter "<> show punterId <>" scored "<> show scoreLocal <>" points"

  Control.Exception.catch
    (punterLoop initGame hdl)
    (\e -> putStrLn $ show punterId <> ": " <> show (e :: SomeException))
```

```haskell
-- assume: every punter already has an entry in the matrix
claimEdge :: PunterId -> Edge -> Game -> Game
claimEdge punterId edge game = M.adjust (edge:) punterId game
```

```haskell
decodePunterInfo :: ByteString -> (PunterId, Int)
decodePunterInfo msg =
  let (punterId, numPunters) = BS.breakSubstring " of " (stripLabel "hello punter:" msg)
  in  (read . BS.unpack $ punterId
      ,read . BS.unpack . stripLabel " of " $ numPunters)

decodeGraph :: ByteString -> Graph
decodeGraph = decodeJSON . stripLabel "graph:"

decodeMsg :: ByteString -> Msg
decodeMsg "move:?" = Query
decodeMsg msg
  | "move:"  `BS.isPrefixOf` msg = Info (decodeJSON (stripLabel "move:" msg))
  | "score:" `BS.isPrefixOf` msg = End (decodeJSON (stripLabel "score:" msg))
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
