{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Util.Proxy.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Monad.State.Strict     (modify)
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as S
import           Data.CaseInsensitive           (CI (..))
import qualified Data.Map                       as Map
import           Snap.Core                      (Request (rqClientAddr, rqClientPort), Snap, rqRemotePort, withRequest)
import           Snap.Test                      (RequestBuilder, evalHandler, get, setHeader)
import           Snap.Test.Common               (coverEqInstance, coverOrdInstance, coverReadInstance, coverShowInstance)
import           Snap.Util.Proxy                (ProxyType (NoProxy, X_Forwarded_For), behindProxy)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertEqual)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testNoProxy
        , testForwardedFor
        , testTrivials
        ]


                                ---------------
                                -- Constants --
                                ---------------

------------------------------------------------------------------------------
initialPort :: Int
initialPort = 9999

initialAddr :: ByteString
initialAddr = "127.0.0.1"


                                  -----------
                                  -- Tests --
                                  -----------

------------------------------------------------------------------------------
testNoProxy :: Test
testNoProxy = testCase "proxy/no-proxy" $ do
    a <- evalHandler (mkReq $ forwardedFor ["4.3.2.1"])
                     (behindProxy NoProxy reportRemoteAddr)
    p <- evalHandler (mkReq $ forwardedFor ["4.3.2.1"] >> xForwardedPort [10903])
                     (behindProxy NoProxy reportRemotePort)
    assertEqual "NoProxy leaves request alone" initialAddr a
    assertEqual "NoProxy leaves request alone" initialPort p

    --------------------------------------------------------------------------
    b <- evalHandler (mkReq $ xForwardedFor ["2fe3::d4"])
                     (behindProxy NoProxy reportRemoteAddr)
    assertEqual "NoProxy leaves request alone" initialAddr b

    --------------------------------------------------------------------------
    c <- evalHandler (mkReq $ return ())
                     (behindProxy NoProxy reportRemoteAddr)
    assertEqual "NoProxy leaves request alone" initialAddr c


------------------------------------------------------------------------------
testForwardedFor :: Test
testForwardedFor = testCase "proxy/forwarded-for" $ do
    (a,p) <- evalHandler (mkReq $ return ()) handler
    assertEqual "No Forwarded-For, no change" initialAddr a
    assertEqual "port" initialPort p

    --------------------------------------------------------------------------
    (b,q) <- evalHandler (mkReq $ forwardedFor [ip4]) handler
    assertEqual "Behind 5.6.7.8" ip4 b
    assertEqual "No Forwarded-Port, no port change" initialPort q

    --------------------------------------------------------------------------
    (c,_) <- evalHandler (mkReq $ xForwardedFor [ip4, ip6]) handler
    assertEqual "Behind 23fe::d4" ip6 c

    --------------------------------------------------------------------------
    (d,r) <- evalHandler (mkReq $ xForwardedFor [ip6, ip4] >> xForwardedPort [20202, port]) handler
    assertEqual "Behind 5.6.7.8" ip4 d
    assertEqual "port change" port r

  where
    handler = behindProxy X_Forwarded_For $ do
                  !a <- reportRemoteAddr
                  !p <- reportRemotePort'
                  return $! (a,p)

    ip4     = "5.6.7.8"
    ip6     = "23fe::d4"
    port    = 10101


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "proxy/trivials" $ do
    coverShowInstance NoProxy
    coverReadInstance NoProxy
    coverEqInstance NoProxy
    coverOrdInstance NoProxy


                                ---------------
                                -- Functions --
                                ---------------

------------------------------------------------------------------------------
mkReq :: RequestBuilder IO () -> RequestBuilder IO ()
mkReq m = do
    get "/" Map.empty
    modify $ \req -> req { rqClientAddr = initialAddr
                         , rqClientPort = initialPort
                         }
    m


------------------------------------------------------------------------------
reportRemoteAddr :: Snap ByteString
reportRemoteAddr = withRequest $ \req -> return $ rqClientAddr req


------------------------------------------------------------------------------
reportRemotePort :: Snap Int
reportRemotePort = withRequest $ \req -> return $ rqClientPort req


------------------------------------------------------------------------------
-- Cover deprecated rqRemotePort
reportRemotePort' :: Snap Int
reportRemotePort' = withRequest $ \req -> return $ rqRemotePort req


------------------------------------------------------------------------------
forwardedFor' :: CI ByteString              -- ^ header name
              -> [ByteString]               -- ^ list of "forwarded-for"
              -> RequestBuilder IO ()
forwardedFor' hdr addrs =
    setHeader hdr $ S.intercalate ", " addrs


------------------------------------------------------------------------------
forwardedFor :: [ByteString]
             -> RequestBuilder IO ()
forwardedFor = forwardedFor' "Forwarded-For"


------------------------------------------------------------------------------
xForwardedFor :: [ByteString]
             -> RequestBuilder IO ()
xForwardedFor = forwardedFor' "X-Forwarded-For"


------------------------------------------------------------------------------
xForwardedPort :: [Int]                     -- ^ list of "forwarded-port"
               -> RequestBuilder IO ()
xForwardedPort ports =
    setHeader "X-Forwarded-Port" $ S.intercalate ", " $ map (S.pack . show) $ ports
