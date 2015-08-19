{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Util.GZip.Tests
  ( tests ) where
------------------------------------------------------------------------------
import qualified Codec.Compression.GZip               as GZip
import qualified Codec.Compression.Zlib               as Zlib
import           Control.Monad                        (replicateM)
import           Data.Bits                            ((.&.))
import qualified Data.ByteString                      as B
import           Data.ByteString.Builder              (byteString)
import           Data.ByteString.Char8                (ByteString)
import qualified Data.ByteString.Char8                as S
import qualified Data.ByteString.Lazy.Char8           as L
import           Data.CaseInsensitive                 (CI)
import           Data.Word                            (Word8)
import           Snap.Core                            (Request, Response, Snap, getHeader, modifyResponse, runSnap, setContentType, setHeader, setResponseBody, writeBS)
import qualified Snap.Test                            as Test
import           Snap.Test.Common                     (coverTypeableInstance, expectException, expectExceptionH, liftQ)
import           Snap.Util.GZip                       (BadAcceptEncodingException, noCompression, withCompression)
import qualified System.IO.Streams                    as Streams
import           System.Random                        (Random (randomIO))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (assertEqual)
import qualified Test.HUnit                           as H
import           Test.QuickCheck                      (Arbitrary (arbitrary))
import           Test.QuickCheck.Monadic              (PropertyM, assert, forAllM, monadicIO)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative                  ((<$>))
#endif
------------------------------------------------------------------------------

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testIdentity1
        , testIdentity1_charset
        , testIdentity2
        , testIdentity3
        , testIdentity4
        , testIdentity5
        , testNoHeaders
        , testNoAcceptEncoding
        , testAcceptEncodingBad
        , testNopWhenContentEncodingSet
        , testCompositionDoesn'tExplode
        , testGzipLotsaChunks
        , testNoCompression
        , testBadHeaders
        , testTrivials
        ]


------------------------------------------------------------------------------
gzipHdrs, xGzipHdrs, xGzipHdrs2, badHdrs, deflateHdrs, xDeflateHdrs
    :: (CI ByteString, ByteString)

gzipHdrs     = ("Accept-Encoding", "deflate,froz,gzip,glorble, x-gzip" )
xGzipHdrs    = ("Accept-Encoding", "x-gzip;q=1.0"      )
xGzipHdrs2   = ("Accept-Encoding", "x-gzip;q=1"        )
badHdrs      = ("Accept-Encoding", "*&%^&^$%&%&*^\023" )
deflateHdrs  = ("Accept-Encoding", "deflate"           )
xDeflateHdrs = ("Accept-Encoding", "x-deflate"         )


------------------------------------------------------------------------------
mkNoHeaders :: IO Request
mkNoHeaders = Test.buildRequest $ return ()


------------------------------------------------------------------------------
mkGzipRq :: IO Request
mkGzipRq = Test.buildRequest $ uncurry Test.setHeader gzipHdrs


------------------------------------------------------------------------------
mkXGzipRq :: IO Request
mkXGzipRq = Test.buildRequest $ uncurry Test.setHeader xGzipHdrs


------------------------------------------------------------------------------
mkXGzip2Rq :: IO Request
mkXGzip2Rq = Test.buildRequest $ uncurry Test.setHeader xGzipHdrs2


------------------------------------------------------------------------------
mkDeflateRq :: IO Request
mkDeflateRq = Test.buildRequest $ uncurry Test.setHeader deflateHdrs


------------------------------------------------------------------------------
mkXDeflateRq :: IO Request
mkXDeflateRq = Test.buildRequest $ uncurry Test.setHeader xDeflateHdrs


------------------------------------------------------------------------------
mkBadRq :: IO Request
mkBadRq = Test.buildRequest $ uncurry Test.setHeader badHdrs


------------------------------------------------------------------------------
seqSnap :: Snap a -> Snap a
seqSnap m = do
    !x <- m
    return $! x `seq` x


------------------------------------------------------------------------------
goGeneric :: IO Request -> Snap a -> IO (Request, Response)
goGeneric mkRq m = do
    rq <- mkRq
    runSnap (seqSnap m) d d rq

  where
    d = (const $ return ())


goGZip, goDeflate, goXGZip, goXGZip2 :: Snap a -> IO (Request,Response)
goNoHeaders, goXDeflate, goBad :: Snap a -> IO (Request,Response)

goGZip      = goGeneric mkGzipRq
goDeflate   = goGeneric mkDeflateRq
goXGZip     = goGeneric mkXGzipRq
goXGZip2    = goGeneric mkXGzip2Rq
goXDeflate  = goGeneric mkXDeflateRq
goBad       = goGeneric mkBadRq
goNoHeaders = goGeneric mkNoHeaders


------------------------------------------------------------------------------
noContentType :: L.ByteString -> Snap ()
noContentType body = modifyResponse $ setResponseBody e
  where
    e s = do
        Streams.writeList (map byteString $ L.toChunks body) s
        return s

------------------------------------------------------------------------------
withContentType :: ByteString -> L.ByteString -> Snap ()
withContentType ct body = modifyResponse $
                          setResponseBody e . setContentType ct
  where
    e s = do
        Streams.writeList (map byteString $ L.toChunks body) s
        return s


------------------------------------------------------------------------------
textPlain :: L.ByteString -> Snap ()
textPlain = withContentType "text/plain"


------------------------------------------------------------------------------
binary :: L.ByteString -> Snap ()
binary = withContentType "application/octet-stream"


------------------------------------------------------------------------------
testNoHeaders :: Test
testNoHeaders = testProperty "gzip/noheaders" $
                monadicIO $
                forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        -- if there's no content-type, withCompression should be a no-op
        (!_,!rsp) <- goNoHeaders (seqSnap $ withCompression
                                          $ noContentType s)
        assertEqual "" Nothing $ getHeader "Content-Encoding" rsp
        assertEqual "" Nothing $ getHeader "Vary" rsp
        body <- Test.getResponseBody rsp

        assertEqual "" s $ L.fromChunks [body]


------------------------------------------------------------------------------
testNoAcceptEncoding :: Test
testNoAcceptEncoding = testProperty "gzip/noAcceptEncoding" $
                       monadicIO $
                       forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        -- if there's no accept-encoding, withCompression should be a no-op
        (!_,!rsp) <- goNoHeaders (seqSnap $ withCompression
                                          $ textPlain s)
        assertEqual "" Nothing $ getHeader "Content-Encoding" rsp
        assertEqual "" Nothing $ getHeader "Vary" rsp

        body <- Test.getResponseBody rsp
        assertEqual "" s $ L.fromChunks [body]


------------------------------------------------------------------------------
testAcceptEncodingBad :: Test
testAcceptEncodingBad = testCase "gzip/acceptEncodingBad" $ do
    expectExceptionH $ Test.runHandler (Test.setHeader "Accept-Encoding" "$")
                                       snap
    expectExceptionH $ Test.runHandler
                         (Test.setHeader "Accept-Encoding" "gzip;q=^") snap
  where
    snap = withCompression $ do
               modifyResponse $ setHeader "Content-Type" "text/plain"
               writeBS "ok"


------------------------------------------------------------------------------
testIdentity1 :: Test
testIdentity1 = testProperty "gzip/identity1" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goGZip (seqSnap $ withCompression $ textPlain s)
        assertEqual "" (Just "gzip") $ getHeader "Content-Encoding" rsp
        assertEqual "" (Just "Accept-Encoding") $ getHeader "Vary" rsp

        body <- Test.getResponseBody rsp
        let s1 = GZip.decompress $ L.fromChunks [body]
        assertEqual "" s s1


------------------------------------------------------------------------------
testIdentity1_charset :: Test
testIdentity1_charset = testProperty "gzip/identity1_charset" $
                        monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goGZip (seqSnap $ withCompression $
                             withContentType "text/plain; charset=utf-8" s)
        assertEqual "" (Just "gzip") $ getHeader "Content-Encoding" rsp
        assertEqual "" (Just "Accept-Encoding") $ getHeader "Vary" rsp

        body <- Test.getResponseBody rsp
        let s1 = GZip.decompress $ L.fromChunks [body]
        assertEqual "" s s1


------------------------------------------------------------------------------
testIdentity2 :: Test
testIdentity2 = testProperty "gzip/identity2" $ monadicIO $
                forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goDeflate (seqSnap $ withCompression $ textPlain s)

        assertEqual "" (Just "deflate") $ getHeader "Content-Encoding" rsp
        assertEqual "" (Just "Accept-Encoding") $ getHeader "Vary" rsp

        body <- Test.getResponseBody rsp
        let s' = Zlib.decompress $ L.fromChunks [body]
        assertEqual "" s s'


------------------------------------------------------------------------------
testIdentity3 :: Test
testIdentity3 = testProperty "gzip/identity3" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goGZip (seqSnap $ withCompression $ binary s)
        body <- Test.getResponseBody rsp
        assertEqual "identify" s $ L.fromChunks [body]


------------------------------------------------------------------------------
testIdentity4 :: Test
testIdentity4 = testProperty "gzip/identity4" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goXGZip (seqSnap $ withCompression $ textPlain s)
        assertEqual "encoding" (Just "x-gzip") (getHeader "Content-Encoding" rsp)
        body <- Test.getResponseBody rsp
        let s1 = GZip.decompress $ L.fromChunks [body]
        assertEqual "identity" s s1

        (!_,!rsp2) <- goXGZip2 (seqSnap $ withCompression $ textPlain s)
        assertEqual "encoding" (Just "x-gzip") (getHeader "Content-Encoding" rsp2)
        body2 <- Test.getResponseBody rsp2
        let s2 = GZip.decompress $ L.fromChunks [body2]
        assertEqual "identity2" s s2


------------------------------------------------------------------------------
testIdentity5 :: Test
testIdentity5 = testProperty "gzip/identity5" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goXDeflate (seqSnap $ withCompression $ textPlain s)

        assertEqual "" (Just "x-deflate") $ getHeader "Content-Encoding" rsp
        body <- Test.getResponseBody rsp
        let s2 = Zlib.decompress $ L.fromChunks [body]
        assertEqual "gzip" s s2


------------------------------------------------------------------------------
testBadHeaders :: Test
testBadHeaders = testProperty "gzip/bad headers" $ monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = expectException $ do
        (!_,!rsp) <- goBad (seqSnap $ withCompression $ textPlain s)
        Test.getResponseBody rsp


------------------------------------------------------------------------------
testNopWhenContentEncodingSet :: Test
testNopWhenContentEncodingSet =
    testProperty "gzip/testNopWhenContentEncodingSet" $
                 monadicIO $
                 forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = do
        (!_,!rsp) <- liftQ $ goGZip $ f s
        assert $ getHeader "Content-Encoding" rsp == Just "identity"

    f !s = seqSnap $ withCompression $ do
            modifyResponse $ setHeader "Content-Encoding" "identity"
            textPlain s


------------------------------------------------------------------------------
testCompositionDoesn'tExplode :: Test
testCompositionDoesn'tExplode =
    testProperty "gzip/testCompositionDoesn'tExplode" $
                 monadicIO $
                 forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goGZip (seqSnap $
                             withCompression $
                             withCompression $
                             withCompression $ textPlain s)

        assertEqual "" (Just "gzip") $ getHeader "Content-Encoding" rsp

        c <- Test.getResponseBody rsp
        let s1 = GZip.decompress $ L.fromChunks [c]
        assertEqual "composition" s s1


------------------------------------------------------------------------------
testGzipLotsaChunks :: Test
testGzipLotsaChunks = testCase "gzip/lotsOfChunks" prop
  where
    prop = do
        a <- genRandom 12000
        let s = L.take 120000 $ L.cycle $ L.fromChunks [a, B.reverse a]
        (!_,!rsp) <- goGZip (seqSnap $ withCompression $ textPlain s)

        body <- Test.getResponseBody rsp
        let s1 = GZip.decompress $ L.fromChunks [body]
        H.assertEqual "streams equal" s s1

    genRandom n = B.pack <$> replicateM n randomWord8

    t8 c = toEnum $ c .&. 0xff

    randomWord8 :: IO Word8
    randomWord8 = t8 <$> randomIO


------------------------------------------------------------------------------
testNoCompression :: Test
testNoCompression = testProperty "gzip/noCompression" $
                    monadicIO $ forAllM arbitrary prop
  where
    prop :: L.ByteString -> PropertyM IO ()
    prop s = liftQ $ do
        (!_,!rsp) <- goGZip (seqSnap $ withCompression $
                             (noCompression >> textPlain s))
        assertEqual "" (Just "identity") $ getHeader "Content-Encoding" rsp
        body <- Test.getResponseBody rsp
        assertEqual "body matches" (S.concat $ L.toChunks s) body


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "gzip/trivials" $ do
    coverTypeableInstance (undefined :: BadAcceptEncodingException)
