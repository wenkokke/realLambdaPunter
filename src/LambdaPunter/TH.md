```haskell
module LambdaPunter.TH where
```

```haskell
import Data.Char (isLower,toLower)
```

```haskell
dropFirstWord :: String -> String
dropFirstWord lbl = let (x:xs) = dropWhile isLower lbl in toLower x : xs
```
