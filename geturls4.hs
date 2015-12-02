#!/usr/bin/env stack
-- stack --resolver lts-3.15 --install-ghc runghc --package HTTP
{-# OPTIONS_GHC -Wall #-}

import GetURL

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Our Async API:

-- <<async
data Async a = Async (MVar (Either SomeException a)) -- <1>

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  _ <- forkIO (do r <- try action; putMVar var r)  -- <2>
  return (Async var)

waitCatch :: Async a -> IO (Either SomeException a) -- <3>
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a -- <4>
wait a = do
  r <- waitCatch a
  case r of
    Left e  -> throwIO e
    Right x -> return x
-- >>

-----------------------------------------------------------------------------

-- <<main
main :: IO ()
main = do
  a1 <- async (getURL "http://yandex.ru")
  a2 <- async (getURL "http://google.com")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
-- >>
