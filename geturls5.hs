#!/usr/bin/env stack
-- stack --resolver lts-3.15 --install-ghc runghc --package HTTP
{-# OPTIONS_GHC -Wall #-}

import Control.Concurrent
import GetURL
import qualified Data.ByteString as B
import Text.Printf
import Control.Monad

-- <<main
sites :: [[Char]]
sites = [
         "http://www.bing.com",
         "http://www.google.com",
         "http://yandex.ru",
         "http://chimera.labs.oreilly.com/books/1230000000929/ch08.html",
         "http://chimera.labs.oreilly.com/books/1230000000929/ch09.html"
         -- "http://www.wikipedia.com/wiki/Spade",
         -- "http://www.wikipedia.com/wiki/Shovel"
        ]

main :: IO ()
main = do
  m <- newEmptyMVar
  let
    download url = do
       r <- getURL url
       putMVar m (url, r)

  mapM_ (forkIO . download) sites

  (url, r) <- takeMVar m
  printf "%s was first (%d bytes)\n" url (B.length r)
  replicateM_ (length sites - 1) (takeMVar m)
-- >>

