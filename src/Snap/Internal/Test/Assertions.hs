{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Test.Assertions where

import           Data.ByteString (ByteString) 
import           Data.Maybe (fromJust)
import           Test.HUnit (Assertion, assertBool, assertEqual)
import           Snap.Internal.Http.Types (Response(..), getHeader)
import           Snap.Internal.Test.RequestBuilder (HasBody (..), getBody)
import           Text.Regex.Posix ((=~))

------------------------------------------------------------------------------
-- | Given a Response returned by an Snap Action, it checks that the Response 
-- wars returned with a HTTP Status 200 code (Success).
assertSuccess :: Response ->  -- ^ A Snap Response returned by an Action
                 Assertion    -- ^ An Assertion result
assertSuccess rsp = assertEqual ("Expected Success (202) but wasn't (" ++ (show status) ++ ")") 200 status
  where status = rspStatus rsp

------------------------------------------------------------------------------
-- | Given a Response returned by an Snap Action, it checks that the Response 
-- was returned with a HTTP Status 404 code (NotFound).
assert404 :: Response ->  -- ^ A Snap Response returned by an Action
             Assertion    -- ^ An Assertion result
assert404 rsp = assertEqual ("Expected Not Found (404) but wasn't (" ++ (show status) ++ ")") 404 status
  where status = rspStatus rsp


------------------------------------------------------------------------------
-- | Given a Response returned by an Snap Action, it checks that the Response 
-- was return with an HTTP Status between 300 and 399 (Redirect) and the
-- Location header of the Response points to the specified URI
assertRedirectTo :: ByteString ->  -- ^ URI where the response should be redirect
                    Response   ->  -- ^ A Snap Response return by an Action
                    Assertion      -- ^ An Assertion result
assertRedirectTo uri rsp = do
  assertRedirect rsp
  let rspUri = fromJust $ getHeader "Location" rsp
  assertEqual ("Expected redirect to " ++ (show uri) ++ " but got redirected to " ++ (show rspUri) ++ " instead")
              uri
              rspUri

------------------------------------------------------------------------------
-- | Given a Response returned by an Snap Action, it checks that the Response 
-- was return with an HTTP Status between 300 and 399 (Redirect) 
assertRedirect :: Response -> -- ^ A Snap Response returned by an Action
                  Assertion   -- ^ An Assertion result
assertRedirect rsp = do 
  let status = rspStatus rsp
  assertBool ("Expected redirect but wasn't (" ++ (show status) ++ ")")
             (status `elem` [300..399])

------------------------------------------------------------------------------
-- | Given a Response returned by an Snap Action, it checks that the Response 
-- body contains a value that match with given ByteString, this ByteString
-- can be a Regexp
assertBodyContains :: (HasBody r) => 
                      ByteString ->  -- ^ Regexp that will match the body content
                      r          ->  -- ^ A Snap Response returned by an Action
                      Assertion      -- ^ An Assertion Result
assertBodyContains match rsp = do
  body <- getBody rsp
  assertBool ("Expected body to match " ++ (show match) ++ " but didn't") (body =~ match)
  
