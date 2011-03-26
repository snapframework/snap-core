{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Test.Assertions where

import           Data.ByteString (ByteString)
import           Data.Maybe (fromJust)
import           Test.HUnit (Assertion, assertBool, assertEqual)
import           Snap.Internal.Http.Types (Response(..), getHeader)
import           Snap.Internal.Test.RequestBuilder (HasBody (..), getBody)
import           Text.Regex.Posix ((=~))

------------------------------------------------------------------------------
-- | Given a Response, asserts that its HTTP status code is 200 (success).
assertSuccess :: Response -> Assertion
assertSuccess rsp = assertEqual message 200 status
  where
    message = "Expected success (200) but got (" ++ (show status) ++ ")"
    status  = rspStatus rsp

------------------------------------------------------------------------------
-- | Given a Response, asserts that its HTTP status code is 404 (Not Found).
assert404 :: Response -> Assertion
assert404 rsp = assertEqual message 404 status
  where
    message = "Expected Not Found (404) but got (" ++ (show status) ++ ")"
    status = rspStatus rsp


------------------------------------------------------------------------------
-- | Given a Response, asserts that its HTTP status code is between 300 and 399
-- (a redirect), and that the Location header of the Response points to the
-- specified URI.
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
-- | Given a Response, asserts that its HTTP status code is between 300 and 399
-- (a redirect).
assertRedirect :: Response -> Assertion
assertRedirect rsp = assertBool message (300 <= status && status <= 399)
  where
    message = "Expected redirect but got status code ("
              ++ show status ++ ")"
    status  = rspStatus rsp


------------------------------------------------------------------------------
-- | Given a Request or a Response, asserts that its body matches the given
-- regular expression.
assertBodyContains :: (HasBody r) =>
                      ByteString   -- ^ Regexp that will match the body content
                   -> r            -- ^ A 'Request' or 'Response'
                   -> Assertion
assertBodyContains match rsp = do
    body <- getBody rsp
    assertBool message (body =~ match)
  where
    message = "Expected body to match regexp \"" ++ show match
              ++ "\", but didn't"
