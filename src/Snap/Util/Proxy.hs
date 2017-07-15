{-# LANGUAGE CPP               #-}
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
import           Control.Applicative   (Alternative ((<|>)))
import           Control.Monad         (mfilter)
import qualified Data.ByteString.Char8 as S (breakEnd, dropWhile, null, readInt, spanEnd)
import           Data.Char             (isSpace)
import           Data.Maybe            (fromMaybe)
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
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> let r = T.get \"\/foo\" M.empty >> T.addHeader \"X-Forwarded-For\" \"1.2.3.4\"
-- ghci> let h = 'Snap.Core.getsRequest' 'rqClientAddr' >>= 'Snap.Core.writeBS')
-- ghci> T.runHandler r h
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Fri, 08 Aug 2014 14:32:29 GMT
--
-- 127.0.0.1
-- ghci> T.runHandler r ('behindProxy' 'X_Forwarded_For' h)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Fri, 08 Aug 2014 14:33:02 GMT
--
-- 1.2.3.4
-- @
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
    extract = fst . S.spanEnd isSpace . S.dropWhile isSpace . snd . S.breakEnd (== ',')

    ip      = fromMaybe (rqClientAddr req) $ mfilter (not . S.null) $ fmap extract $
              getHeader "Forwarded-For"   req  <|>
              getHeader "X-Forwarded-For" req

    port    = maybe (rqClientPort req) fst $ (S.readInt =<<) $ fmap extract $
              getHeader "Forwarded-Port"   req  <|>
              getHeader "X-Forwarded-Port" req
{-# INLINE xForwardedFor #-}
