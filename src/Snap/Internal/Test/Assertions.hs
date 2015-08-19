{-# LANGUAGE CPP, OverloadedStrings #-}
module Snap.Internal.Test.Assertions where

------------------------------------------------------------------------------
import           Control.Monad              (liftM)
import           Data.ByteString.Builder    (toLazyByteString)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe                 (fromJust)
import           Snap.Internal.Http.Types   (Response (rspBody, rspStatus), getHeader, rspBodyToEnum)
import qualified System.IO.Streams          as Streams
import           Test.HUnit                 (Assertion, assertBool, assertEqual)
import           Text.Regex.Posix           ((=~))
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid                (mconcat)
#endif
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Given a 'Response', return its body as a 'ByteString'.
--
-- Example:
--
-- @
-- ghci> 'getResponseBody' 'emptyResponse'
-- \"\"
-- @
--
getResponseBody :: Response -> IO ByteString
getResponseBody rsp = do
    (os, grab) <- Streams.listOutputStream
    enum os
    liftM toBS grab

  where
    enum os = do
        os' <- rspBodyToEnum (rspBody rsp) os
        Streams.write Nothing os'

    toBS = S.concat . L.toChunks . toLazyByteString . mconcat


------------------------------------------------------------------------------
-- | Given a 'Response', assert that its HTTP status code is 200 (success).
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Test.HUnit" as T
-- ghci> let test = T.runTestTT . T.TestCase
-- ghci> test $ 'assertSuccess' 'Snap.Core.emptyResponse'
-- Cases: 1  Tried: 1  Errors: 0  Failures: 0
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
-- ghci> test $ 'assertSuccess' ('Snap.Core.setResponseStatus' 500 \"Internal Server Error\" 'Snap.Core.emptyResponse')
-- ### Failure:
-- Expected success (200) but got (500)
-- expected: 200
--  but got: 500
-- Cases: 1  Tried: 1  Errors: 0  Failures: 1
-- Counts {cases = 1, tried = 1, errors = 0, failures = 1}
-- @
assertSuccess :: Response -> Assertion
assertSuccess rsp = assertEqual message 200 status
  where
    message = "Expected success (200) but got (" ++ (show status) ++ ")"
    status  = rspStatus rsp


------------------------------------------------------------------------------
-- | Given a 'Response', assert that its HTTP status code is 404 (Not Found).
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> 'assert404' $ 'Snap.Core.setResponseStatus' 404 \"Not Found\" 'Snap.Core.emptyResponse'
-- ghci> 'assert404' 'Snap.Core.emptyResponse'
-- *** Exception: HUnitFailure \"Expected Not Found (404) but got (200)\\nexpected: 404\\n but got: 200\"
-- @
assert404 :: Response -> Assertion
assert404 rsp = assertEqual message 404 status
  where
    message = "Expected Not Found (404) but got (" ++ (show status) ++ ")"
    status = rspStatus rsp


------------------------------------------------------------------------------
-- | Given a 'Response', assert that its HTTP status code is between 300 and 399
-- (a redirect), and that the Location header of the 'Response' points to the
-- specified URI.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> let r' = 'Snap.Core.setResponseStatus' 301 \"Moved Permanently\" 'Snap.Core.emptyResponse'
-- ghci> let r  = 'Snap.Core.setHeader' \"Location\" \"www.example.com\" r'
-- ghci> 'assertRedirectTo' \"www.example.com\" r
-- ghci> 'assertRedirectTo' \"www.example.com\" 'Snap.Core.emptyResponse'
-- *** Exception: HUnitFailure \"Expected redirect but got status code (200)\"
-- @
assertRedirectTo :: ByteString     -- ^ The Response should redirect to this
                                   -- URI
                 -> Response
                 -> Assertion
assertRedirectTo uri rsp = do
    assertRedirect rsp
    assertEqual message uri rspUri

  where
    rspUri = fromJust $ getHeader "Location" rsp
    message = "Expected redirect to " ++ show uri
              ++ " but got redirected to "
              ++ show rspUri ++ " instead"


------------------------------------------------------------------------------
-- | Given a 'Response', assert that its HTTP status code is between 300 and 399
-- (a redirect).
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> 'assertRedirect' $ 'Snap.Core.setResponseStatus' 301 \"Moved Permanently\" 'Snap.Core.emptyResponse'
-- ghci> 'assertRedirect' 'Snap.Core.emptyResponse'
-- *** Exception: HUnitFailure \"Expected redirect but got status code (200)\"
-- @
assertRedirect :: Response -> Assertion
assertRedirect rsp = assertBool message (300 <= status && status <= 399)
  where
    message = "Expected redirect but got status code ("
              ++ show status ++ ")"
    status  = rspStatus rsp


------------------------------------------------------------------------------
-- | Given a 'Response', assert that its body matches the given regular
-- expression.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "System.IO.Streams" as Streams
-- ghci> import qualified "Data.ByteString.Builder" as Builder
-- ghci> :{
-- ghci| let r = 'Snap.Core.setResponseBody'
-- ghci|         (\out -> do
-- ghci|             Streams.write (Just $ Builder.byteString \"Hello, world!\") out
-- ghci|             return out)
-- ghci|         'Snap.Core.emptyResponse'
-- ghci| :}
-- ghci> 'assertBodyContains' \"^Hello\" r
-- ghci> 'assertBodyContains' \"Bye\" r
-- *** Exception: HUnitFailure "Expected body to match regexp \\\"\\\"Bye\\\"\\\", but didn\'t"
-- @
assertBodyContains :: ByteString  -- ^ Regexp that will match the body content
                   -> Response
                   -> Assertion
assertBodyContains match rsp = do
    body <- getResponseBody rsp
    assertBool message (body =~ match)
  where
    message = "Expected body to match regexp \"" ++ show match
              ++ "\", but didn't"
