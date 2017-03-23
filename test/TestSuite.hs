module Main where

import           Test.Framework                 (defaultMain, testGroup)

------------------------------------------------------------------------------
import qualified Snap.Core.Tests
import qualified Snap.Internal.Http.Types.Tests
import qualified Snap.Internal.Parsing.Tests
import qualified Snap.Internal.Routing.Tests
import qualified Snap.Test.Tests
import qualified Snap.Types.Headers.Tests
import qualified Snap.Util.FileServe.Tests
import qualified Snap.Util.FileUploads.Tests
import qualified Snap.Util.GZip.Tests
import qualified Snap.Util.Proxy.Tests
import qualified Snap.Util.CORS.Tests


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Snap.Internal.Http.Types.Tests"
                        Snap.Internal.Http.Types.Tests.tests
            , testGroup "Snap.Internal.Routing.Tests"
                        Snap.Internal.Routing.Tests.tests
            , testGroup "Snap.Core.Tests"
                        Snap.Core.Tests.tests
            , testGroup "Snap.Internal.Parsing.Tests"
                        Snap.Internal.Parsing.Tests.tests
            , testGroup "Snap.Types.Headers.Tests"
                        Snap.Types.Headers.Tests.tests
            , testGroup "Snap.Util.FileServe.Tests"
                        Snap.Util.FileServe.Tests.tests
            , testGroup "Snap.Util.FileUploads.Tests"
                        Snap.Util.FileUploads.Tests.tests
            , testGroup "Snap.Util.GZip.Tests"
                        Snap.Util.GZip.Tests.tests
            , testGroup "Snap.Util.Proxy.Tests"
                        Snap.Util.Proxy.Tests.tests
            , testGroup "Snap.Util.CORS.Tests"
                        Snap.Util.CORS.Tests.tests
            , testGroup "Snap.Test.Tests"
                        Snap.Test.Tests.tests
            ]
