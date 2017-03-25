{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE OverloadedStrings #-}

module Snap.Util.CORS.Tests (tests) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8          (ByteString)
import           Data.CaseInsensitive           (CI (..))
import qualified Data.HashSet                   as HashSet
import           Snap.Core                      (Method (..), getHeader, Response(..))
import           Snap.Test                      (RequestBuilder, runHandler, setHeader, setRequestType, RequestType(..), setRequestPath)
import           Snap.Util.CORS                 (applyCORS,CORSOptions(..),defaultOptions,HashableMethod(..))
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertEqual,Assertion)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCORSSimple
        , testCORSOptions
        ]


                                ---------------
                                -- Constants --
                                ---------------

------------------------------------------------------------------------------
origin :: ByteString
origin = "http://origin.org"



                                  -----------
                                  -- Tests --
                                  -----------

------------------------------------------------------------------------------
testCORSSimple :: Test
testCORSSimple = testCase "CORS/simple" $ do
    let testDefault meth = do
          r <- runHandler (mkMethReq meth) $
               applyCORS defaultOptions $ return ()
          checkAllowOrigin (Just origin) r
          checkAllowCredentials (Just "true") r
          checkExposeHeaders Nothing r
    mapM_ testDefault [GET,POST,PUT,DELETE,HEAD]

------------------------------------------------------------------------------
testCORSOptions :: Test
testCORSOptions = testCase "CORS/options" $ do
  let opts = applyCORS defaultOptions { corsAllowedMethods =
               return $ HashSet.singleton $ HashableMethod GET  }
  r <- runHandler (mkMethReq OPTIONS
                   >> setRequestMethod "GET"
                   >> setRequestHeaders "X-STUFF, Content-Type") $
       opts $ return ()
  checkAllowOrigin (Just origin) r
  checkAllowCredentials (Just "true") r
  checkAllowHeaders (Just "X-STUFF, Content-Type") r
  checkAllowMethods (Just "GET") r
  ---------------------------------------------------------
  s <- runHandler (mkMethReq OPTIONS
                   >> setRequestMethod "POST"
                   >> setRequestHeaders "X-STUFF, Content-Type") $
       opts $ return ()
  checkAllowOrigin Nothing s
  checkAllowCredentials Nothing s
  checkAllowHeaders Nothing s
  checkAllowMethods Nothing s


                                ---------------
                                -- Functions --
                                ---------------

------------------------------------------------------------------------------
mkMethReq :: Method -> RequestBuilder IO ()
mkMethReq meth = do
  setRequestType $ RequestWithRawBody meth ""
  setRequestPath "/"
  setHeader "Origin" origin

checkHeader :: CI ByteString -> Maybe ByteString -> Response -> Assertion
checkHeader h v r = assertEqual ("Header " ++ show h) v (getHeader h r)

checkAllowOrigin :: Maybe ByteString -> Response -> Assertion
checkAllowOrigin = checkHeader "Access-Control-Allow-Origin"

checkAllowCredentials :: Maybe ByteString -> Response -> Assertion
checkAllowCredentials = checkHeader "Access-Control-Allow-Credentials"

checkExposeHeaders :: Maybe ByteString -> Response -> Assertion
checkExposeHeaders = checkHeader "Access-Control-Expose-Headers"

checkAllowHeaders :: Maybe ByteString -> Response -> Assertion
checkAllowHeaders = checkHeader "Access-Control-Allow-Headers"

checkAllowMethods :: Maybe ByteString -> Response -> Assertion
checkAllowMethods = checkHeader "Access-Control-Allow-Methods"

setRequestMethod :: ByteString -> RequestBuilder IO ()
setRequestMethod = setHeader "Access-Control-Request-Method"

setRequestHeaders :: ByteString -> RequestBuilder IO ()
setRequestHeaders = setHeader "Access-Control-Request-Headers"
