#!/usr/bin/env stack
-- stack --resolver lts-3.15 --install-ghc runghc --package HTTP

{-# OPTIONS_GHC -Wall #-}

-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.
--
-- Sample geturls.hs (CEFP summer school notes, 2011)
--
-- Downloading multiple URLs concurrently, timing the downloads,
-- and the user may press 'q' to stop the downloads at any time.
--
-- Compile with:
--    ghc -threaded --make geturlscancel.hs

import GetURL
import TimeIt

import Data.Either
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Exception
import Text.Printf
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Our Async API:

-- <<Async
data Async a = Async ThreadId (MVar (Either SomeException a))
-- >>

-- <<async
-- is it racy?
async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- forkIO $ do
      r <- try action
      -- threadDelay 1000000
      putMVar m r
   return (Async t m)
-- >>

-- is it racy?
async'' :: IO a -> IO (Async a)
async'' action = do
   m <- newEmptyMVar
   t <- forkIO $ do
      r <- try $ do
        ret <- action
        threadDelay 1000000
        return ret
      putMVar m r
   return (Async t m)

-- should be OK
async''' :: IO a -> IO (Async a)
async''' action = do
   m <- newEmptyMVar
   t <- mask_ $ forkIOWithUnmask $ \restore -> do
      r <- try $ restore action
      uninterruptibleMask_ $ threadDelay 1000000
      putMVar m r
   return (Async t m)

-- should be OK and good?
-- from Control.Exception documentation:
-- There's an implied mask around every exception handler in a call to one of the catch family of functions.
-- This is because that is what you want most of the time - it eliminates a common race condition
-- in starting an exception handler, because there may be no exception handler on the stack to handle another
-- exception if one arrives immediately. If asynchronous exceptions are masked on entering the handler, though,
-- we have time to install a new exception handler before being interrupted.
--
-- UPD: later in the chapter "Catching Asynchronous Exceptions" this explained
-- UPD2: no! an asynchronous exeptions may happen in the childthread just before the first IO action.
async' :: IO a -> IO (Async a)
async' action = do
   m <- newEmptyMVar
   t <- forkIO $
      (do r <- action; threadDelay 1000000; putMVar m (Right r)) `catch` (\ e -> putMVar m (Left (e :: SomeException)))
   return (Async t m)

-- <<waitCatch
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ var) = readMVar var
-- >>

-- <<cancel
cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled
-- >>

-----------------------------------------------------------------------------

sites :: [[Char]]
sites = [
         "http://www.bing.com",
         "http://www.google.com",
         "http://yandex.ru",
         "http://www.yahoo.ru", -- unsuccessful one (https is not supported)
         "http://chimera.labs.oreilly.com/books/1230000000929/ch08.html",
         "http://chimera.labs.oreilly.com/books/1230000000929/ch09.html"
        ]

timeDownload :: String -> IO (B.ByteString, Double)
timeDownload url = timeit $ getURL url

-- <<main
main :: IO ()
main = do
  as <- mapM (async' . timeDownload) sites                     -- <1>

  void $ forkIO $ do                                                 -- <2>
     void $ hSetBuffering stdin NoBuffering
     forever $ do
        c <- getChar
        when (c == 'q') $ mapM_ cancel as

  rs <- mapM waitCatch as                                     -- <3>

  mapM_ f $ zip sites rs

  printf "%d/%d succeeded\n" (length (rights rs)) (length rs) -- <4>

    where
      f (n, (Left e))       = printf "FAIL: %s %s\n" n (show e)
      f (n, (Right (s, t))) = printf "OK:   %s time: %f, length: %d\n" n t (B.length s)
-- >>

