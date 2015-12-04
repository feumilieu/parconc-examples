#!/usr/bin/env stack
-- stack --resolver lts-3.15 --install-ghc runghc --package HTTP

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveDataTypeable #-}
import Control.Concurrent
import Data.Unique
import Control.Exception
import Data.Typeable
import Control.Monad (forever)

data Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
  show (Timeout _) = "timeout"

instance Exception Timeout

-- <<timeout-sig
timeout :: Int -> IO a -> IO (Maybe a)
-- >>

-- <<timeout
timeout t m
    | t <  0    = fmap Just m                           -- <1>
    | t == 0    = return Nothing                        -- <1>
    | otherwise = do
        pid <- myThreadId                               -- <2>
        u <- newUnique                                  -- <3>
        let ex = Timeout u                              -- <3>
        handleJust                                      -- <4>
           (\e -> if e == ex then Just () else Nothing) -- <5>
           (\_ -> return Nothing)                       -- <6>
           (bracket (forkIO $ do threadDelay t          -- <7>
                                 throwTo pid ex)
                    (\tid -> throwTo tid ThreadKilled)  -- <8>
                    (\_ -> fmap Just m))                -- <9>
-- >>

test :: Int -> IO (Maybe (Maybe (Maybe (Maybe (Maybe (Maybe ()))))))
test d = timeout d $ timeout d $ timeout d $ timeout d $ timeout d $ timeout 100000000 $ threadDelay 100

main :: IO ()
-- buggy: see https://ghc.haskell.org/trac/ghc/ticket/7719
-- main = (timeout 200000 $ timeout 100000 $ timeout 300000 $ threadDelay 1000000) >>= print
main = forever $ mapM_ test [1 .. 200]
