{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Util.Proxy.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Monad.State hiding (get)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.CaseInsensitive (CI(..))
import qualified Data.Map as Map
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
------------------------------------------------------------------------------
import           Snap.Core hiding (setHeader)
import           Snap.Test
import           Snap.Test.Common
import           Snap.Util.Proxy
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
    a <- evalHandler (mkReq $ forwardedFor [("4.3.2.1", Nothing)])
                     (behindProxy NoProxy reportRemoteAddr)
    p <- evalHandler (mkReq $ forwardedFor [("4.3.2.1", Nothing)])
                     (behindProxy NoProxy reportRemotePort)
    assertEqual "NoProxy leaves request alone" initialAddr a
    assertEqual "NoProxy leaves request alone" initialPort p

    --------------------------------------------------------------------------
    b <- evalHandler (mkReq $ xForwardedFor [("4.3.2.1", Nothing)])
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
    (b,_) <- evalHandler (mkReq $ forwardedFor addr) handler
    assertEqual "Behind 5.6.7.8" ip b

    --------------------------------------------------------------------------
    (c,q) <- evalHandler (mkReq $ xForwardedFor addrs2) handler
    assertEqual "Behind 5.6.7.8" ip c
    assertEqual "port change" port q

  where
    handler = behindProxy X_Forwarded_For $ do
                  !a <- reportRemoteAddr
                  !p <- reportRemotePort
                  return $! (a,p)

    ip      = "5.6.7.8"
    port    = 10101

    addr    = [ (ip, Nothing) ]

    addr2   = [ (ip, Just port) ]
    addrs2  = [("4.3.2.1", Just 20202)] ++ addr2


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
    modify $ \req -> req { rqRemoteAddr = initialAddr
                         , rqRemotePort = initialPort
                         }
    m


------------------------------------------------------------------------------
reportRemoteAddr :: Snap ByteString
reportRemoteAddr = withRequest $ \req -> return $ rqRemoteAddr req


------------------------------------------------------------------------------
reportRemotePort :: Snap Int
reportRemotePort = withRequest $ \req -> return $ rqRemotePort req


------------------------------------------------------------------------------
forwardedFor' :: CI ByteString              -- ^ header name
              -> [(ByteString, Maybe Int)]  -- ^ list of "forwarded-for"
              -> RequestBuilder IO ()
forwardedFor' hdr addrs = do
    setHeader hdr out

  where
    toStr (a, Nothing) = a
    toStr (a, Just p ) = S.concat [ a, ":", S.pack $ show p ]

    out  = S.intercalate ", " $ map toStr addrs


------------------------------------------------------------------------------
forwardedFor :: [(ByteString, Maybe Int)]
             -> RequestBuilder IO ()
forwardedFor = forwardedFor' "Forwarded-For"


------------------------------------------------------------------------------
xForwardedFor :: [(ByteString, Maybe Int)]
             -> RequestBuilder IO ()
xForwardedFor = forwardedFor' "X-Forwarded-For"
