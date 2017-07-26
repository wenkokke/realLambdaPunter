module Main where

import Control.Concurrent
import Control.Exception
import Data.IORef
import LambdaPunter
import Network.Socket
import System.Environment
import System.IO
import System.IO.Unsafe

main :: IO ()
main = do
  lpHost <- inet_addr "127.0.0.1"
  let lpPort = 9999
  let lpAddr = SockAddrInet lpPort lpHost
  args <- getArgs
  case args of
    ["randy"]    -> do
      connectPunter randy    lpAddr
    ["tortoise"] -> do
      ioRef <- newIORef Nothing
      connectPunter (tortoise ioRef) lpAddr
    ["greedo"]   -> do
      connectPunter greedo   lpAddr
    _            -> do
      putStrLn "usage: realLambdaPunter [randy|tortoise|greedo]"

connectPunter :: Punter -> SockAddr -> IO ()
connectPunter punter addr = do
  sock <- socket AF_INET Stream 0
  connect sock addr
  hdl <- socketToHandle sock ReadWriteMode
  Control.Exception.catch
    (runPunter punter hdl)
    (\e -> print (e :: IOException))
  hClose hdl
