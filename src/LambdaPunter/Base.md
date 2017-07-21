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
import Data.Char (toLower)
import qualified Data.IntMap as M
import Data.List (find)
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

Representing punters and the game state
---

```haskell
type Game = M.IntMap [Edge]
```

```haskell
type PunterId = Int
```

```haskell
type Punter = Graph -> ScoringData -> PunterId -> Game -> IO Edge
```

Representing game modes and the setup
---

```haskell
newtype Mode = Mode { modeMode :: String }
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Mode)
```

```haskell
data Setup = Setup
  { setupPunter  :: PunterId
  , setupPunters :: Int
  }
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Setup)
```

Representing moves
---

```haskell
data Move
  = Claim { movePunter :: PunterId
          , moveSource :: NodeId
          , moveTarget :: NodeId
          }
  | Pass  { passPunter :: PunterId
          }
```

```haskell
$(deriveJSON defaultOptions
  { fieldLabelModifier     = dropFirstWord
  , constructorTagModifier = map toLower
  , sumEncoding            = ObjectWithSingleField
  } ''Move)
```

Representing messages
---

```haskell
data Score = Score
  { scorePunter :: PunterId
  , scoreScore  :: Int
  }

data Msg
  = Move { moveMoves :: [Move] }
  | Stop { stopMoves :: [Move], stopScores :: [Score] }
```

```haskell
$(deriveJSON defaultOptions{fieldLabelModifier = dropFirstWord} ''Score)
$(deriveJSON defaultOptions
  { fieldLabelModifier     = dropFirstWord
  , constructorTagModifier = map toLower
  , sumEncoding            = ObjectWithSingleField
  } ''Msg)
```

Running punters over a handle
---

```haskell
runPunter :: Punter -> Handle -> IO ()
runPunter punter hdl = do

  -- tell the server we wish to punt
  T.hPutStrLn hdl (encodeToLazyText (Mode "punt"))

  -- receive the game setup
  msg <- BS.hGetLine hdl
  let
    Setup punterId numPunters = decodeJSON msg

  -- receive graph
  msg <- BS.hGetLine hdl
  let
    graph       = wrappedGraph $ decodeJSON msg
    scoringData = mkScoringData graph
    realPunter  = punter graph scoringData punterId
    initGame    = foldr (\punterId -> M.insert punterId []) M.empty [0..numPunters - 1]

  -- loop
  let
    punterLoop :: Game -> Handle -> IO ()
    punterLoop game hdl = do
      msg <- BS.hGetLine hdl
      case decodeJSON msg of
        Move moves -> do
          let game'  = foldr (\m f -> runMove m . f) id moves game
          Edge source target <- realPunter game'
          let move   = Claim punterId source target
          let game'' = runMove move game'
          T.hPutStrLn hdl (encodeToLazyText move)
          punterLoop game'' hdl
        Stop moves scores -> do
          let Just scoreRemote = scoreScore <$> find ((== punterId).scorePunter) scores
          let scoreLocal = score graph scoringData (game M.! punterId)
          assert (scoreRemote == scoreLocal) $
            putStrLn $
              "Punter "<> show punterId <>" scored "<> show scoreLocal <>" points"

  Control.Exception.catch
    (punterLoop initGame hdl)
    (\e -> putStrLn $ show punterId <> ": " <> show (e :: SomeException))
```

```haskell
runMove :: Move -> Game -> Game
runMove (Claim punterId source target) = claimEdge punterId (Edge source target)
runMove (Pass _) = id

-- assume: every punter already has an entry in the matrix
claimEdge :: PunterId -> Edge -> Game -> Game
claimEdge punterId edge game = M.adjust (edge:) punterId game
```

```haskell
decodeJSON :: FromJSON a => ByteString -> a
decodeJSON msg =
  case decodeStrict msg of
    Just val -> val
    Nothing  -> throw $ CannotDecode msg
```

```haskell
data PunterException
  = CannotDecode ByteString
  | ExpectedLabel String ByteString
  deriving (Eq,Show)

instance Exception PunterException
```
