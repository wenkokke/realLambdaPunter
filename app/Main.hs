module Main where

import Control.Concurrent
import LambdaPunter
import Network.Socket
import System.IO
import System.IO.Unsafe

-- |Run two players on `127.0.0.1:9999`
main :: IO ()
main = do
  lpHost <- inet_addr "127.0.0.1"
  let lpPort = 9999
  let lpAddr = SockAddrInet lpPort lpHost
  forkChild $ connectPlayer undefined lpAddr 
  forkChild $ connectPlayer undefined lpAddr
  waitForChildren

-- |Run a player on a socket
connectPlayer :: Player -> SockAddr -> IO ()
connectPlayer player addr = do
  sock <- socket AF_INET Stream 0
  connect sock addr
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  runPlayer player hdl
  hClose hdl

-- * Concurrent players

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
       putMVar children ms
       takeMVar m
       waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkFinally io (\_ -> putMVar mvar ())
