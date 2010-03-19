{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- "Snap.Http.Types"
--
-- Basic HTTP data structures and functions.
--
-- TODO: http auth, setters for (some) fields -- use something like
-- fclabels?
--
-- TODO: decide: should Request headers be immutable?
--
-- TODO: haddock documentation

module Snap.Http.Types
  (
  -- * Types
    Enumerator
  , Headers
  , HttpVersion
  , Params
  , Request
  , Response
  , Method(..)
  , HasHeaders(..)
  , Cookie

  -- * FIXME: these don't belong here
  , enumBS
  , enumLBS

  -- * Functions
  , rqServerName
  , rqServerPort
  , rqRemoteAddr
  , rqRemotePort
  , rqLocalAddr
  , rqLocalHostname
  , rqIsSecure
  , rqContentLength
  , rqMethod
  , rqVersion
  , rqCookies
  , rqPathInfo
  , rqContextPath
  , rqURI
  , rqQueryString
  , rqParams
  , rqParam
  , rqModifyParams
  , rqSetParam
  , emptyResponse
  , setResponseBody
  , setResponseStatus
  , rspStatus
  , filterResponseBody
  , setContentType
  , addCookie
  , setContentLength

  ) where

------------------------------------------------------------------------------
import           Snap.Iteratee (enumBS, enumLBS)
import           Snap.Internal.Http.Types

