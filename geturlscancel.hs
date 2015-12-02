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
async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- forkIO (do r <- try action; putMVar m r)
   return (Async t m)
-- >>

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
  as <- mapM (async . timeDownload) sites                     -- <1>

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

