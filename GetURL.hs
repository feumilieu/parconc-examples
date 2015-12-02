-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- Simple wrapper around HTTP, allowing proxy use

module GetURL (getURL) where

import Network.HTTP
import Network.Browser
import Network.URI
import Data.ByteString (ByteString)

getURL :: String -> IO ByteString
getURL url = do
  Network.Browser.browse $ do
    setAllowRedirects True
    setMaxRedirects $ Just 20
    setCheckForProxy True
    -- setDebugLog $ Just "HTTP.log"
    setDebugLog Nothing
    setOutHandler (const (return ()))
    (_, rsp) <- request (getRequest' (escapeURIString isUnescapedInURI url))
    return (rspBody rsp)
  where
   getRequest' :: String -> Request ByteString
   getRequest' urlString =
    case parseURI urlString of
      Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
      Just u  -> mkRequest GET u
