module Main where

import Control.Concurrent
import Control.Exception
import Data.IORef
import LambdaPunter
import Network.Socket
import System.IO
import System.IO.Unsafe

main :: IO ()
main = do
  lpHost <- inet_addr "127.0.0.1"
  let lpPort = 9999
  let lpAddr = SockAddrInet lpPort lpHost
  connectPunter greedo lpAddr

connectPunter :: Punter -> SockAddr -> IO ()
connectPunter punter addr = do
  sock <- socket AF_INET Stream 0
  connect sock addr
  hdl <- socketToHandle sock ReadWriteMode
  Control.Exception.catch
    (runPunter punter hdl)
    (\e -> print (e :: IOException))
  hClose hdl

-- children :: MVar [MVar ()]
-- children = unsafePerformIO (newMVar [])
--
-- waitForChildren :: IO ()
-- waitForChildren = do
--   cs <- takeMVar children
--   case cs of
--     []   -> return ()
--     m:ms -> do
--        putMVar children ms
--        takeMVar m
--        waitForChildren
--
-- forkChild :: IO () -> IO ThreadId
-- forkChild io = do
--     mvar <- newEmptyMVar
--     childs <- takeMVar children
--     putMVar children (mvar:childs)
--     forkFinally io (\_ -> putMVar mvar ())
