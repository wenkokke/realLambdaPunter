@realLambdaPunter
===

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaPunter.Base
  (Game
  ,LegalMoves
  ,PunterId
  ,Punter
  ,Move(..)
  ,Msg(..)
  ,PunterException(..)
  ,runPunter
  ,claimRiver
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
import qualified Data.Set as S
import qualified Data.IntMap as M
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import LambdaPunter.TH (dropFirstWord)
import LambdaPunter.Map as X
import LambdaPunter.Score as X
import System.IO
```

```haskell
import qualified Data.IntMap as M
import LambdaPunter.Map
```

Representing punters and the game state
---

```haskell
type Game = M.IntMap [River]
type LegalMoves = S.Set River
```

```haskell
type PunterId = Int
```

```haskell
type Punter = Map -> ScoringData -> PunterId -> Game -> LegalMoves -> IO River
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
          , moveSource :: SiteId
          , moveTarget :: SiteId
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
    graph       = wrappedMap $ decodeJSON msg
    legalMoves  = S.fromList $ graphRivers graph
    scoringData = mkScoringData graph
    realPunter  = punter graph scoringData punterId
    initGame    = foldr (\punterId -> M.insert punterId []) M.empty [0..numPunters - 1]

  -- loop
  let
    punterLoop :: Game -> LegalMoves -> Handle -> IO ()
    punterLoop game legalMoves hdl = do
      msg <- BS.hGetLine hdl
      case decodeJSON msg of
        Move moves -> do
          let game'  = foldr (\m f -> runMove m . f) id moves game
          let legalMoves' = S.difference legalMoves (S.fromList $ mapMaybe moveToRiver moves)
          River source target <- realPunter game' legalMoves'
          let move   = Claim punterId source target
          let game'' = runMove move game'
          let legalMoves'' = S.delete (River source target) legalMoves'
          T.hPutStrLn hdl (encodeToLazyText move)
          punterLoop game'' legalMoves'' hdl
        Stop moves scores -> do
          let Just scoreRemote = scoreScore <$> find ((== punterId).scorePunter) scores
          let scoreLocal = score graph scoringData (game M.! punterId)
          assert (scoreRemote == scoreLocal) $
            putStrLn $
              "Punter "<> show punterId <>" scored "<> show scoreLocal <>" points"

  Control.Exception.catch
    (punterLoop initGame legalMoves hdl)
    (\e -> putStrLn $ show punterId <> ": " <> show (e :: SomeException))
```

```haskell
moveToRiver :: Move -> Maybe River
moveToRiver (Claim _ source target) = Just $ River source target
moveToRiver (Pass _) = Nothing
```

```haskell
runMove :: Move -> Game -> Game
runMove (Claim punterId source target) = claimRiver punterId (River source target)
runMove (Pass _) = id

-- assume: every punter already has an entry in the matrix
claimRiver :: PunterId -> River -> Game -> Game
claimRiver punterId river game = M.adjust (river:) punterId game
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
