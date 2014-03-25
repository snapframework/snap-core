{-# LANGUAGE OverloadedStrings #-}

-- | This module provides facilities for patching incoming 'Requests' to
-- correct the value of 'rqClientAddr' if the snap server is running behind a
-- proxy.
--
-- Example usage:
--
-- @
-- m :: Snap ()
-- m = undefined  -- code goes here
--
-- applicationHandler :: Snap ()
-- applicationHandler = behindProxy X_Forwarded_For m
-- @
--
module Snap.Util.Proxy
  ( ProxyType(..)
  , behindProxy
  ) where

------------------------------------------------------------------------------
import           Control.Applicative   (Alternative ((<|>)), (<$>))
import           Control.Arrow         (second)
import qualified Data.ByteString.Char8 as S (break, breakEnd, drop, dropWhile, readInt, spanEnd)
import           Data.Char             (isSpace)
import           Data.Maybe            (fromJust)
import           Snap.Core             (MonadSnap, Request (rqClientAddr, rqClientPort), getHeader, modifyRequest)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | What kind of proxy is this? Affects which headers 'behindProxy' pulls the
-- original remote address from.
--
-- Currently only proxy servers that send @X-Forwarded-For@ or @Forwarded-For@
-- are supported.
data ProxyType = NoProxy          -- ^ no proxy, leave the request alone
               | X_Forwarded_For  -- ^ Use the @Forwarded-For@ or
                                  --   @X-Forwarded-For@ header
  deriving (Read, Show, Eq, Ord)


------------------------------------------------------------------------------
-- | Rewrite 'rqClientAddr' if we're behind a proxy.
behindProxy :: MonadSnap m => ProxyType -> m a -> m a
behindProxy NoProxy         = id
behindProxy X_Forwarded_For = ((modifyRequest xForwardedFor) >>)
{-# INLINE behindProxy #-}


------------------------------------------------------------------------------
xForwardedFor :: Request -> Request
xForwardedFor req = req { rqClientAddr = ip
                        , rqClientPort = port
                        }
  where
    proxyString  = getHeader "Forwarded-For"   req <|>
                   getHeader "X-Forwarded-For" req <|>
                   Just (rqClientAddr req)

    proxyAddr    = trim . snd . S.breakEnd (== ',') . fromJust $ proxyString

    trim         = fst . S.spanEnd isSpace . S.dropWhile isSpace

    (ip,portStr) = second (S.drop 1) . S.break (== ':') $ proxyAddr

    port         = fromJust (fst <$> S.readInt portStr <|>
                             Just (rqClientPort req))
{-# INLINE xForwardedFor #-}
