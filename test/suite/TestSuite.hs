module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Snap.Types.Tests
import qualified Snap.Internal.Http.Types.Tests
import qualified Snap.Internal.Routing.Tests
import qualified Snap.Iteratee.Tests
import qualified Snap.Util.FileServe.Tests
import qualified Snap.Util.FileUploads.Tests
import qualified Snap.Util.GZip.Tests
import qualified Snap.Test.RequestBuilder.Tests


main :: IO ()
main = defaultMain tests
  where tests = [
                  testGroup "Snap.Internal.Http.Types.Tests"
                            Snap.Internal.Http.Types.Tests.tests
                , testGroup "Snap.Internal.Routing.Tests"
                            Snap.Internal.Routing.Tests.tests
                , testGroup "Snap.Types.Tests"
                            Snap.Types.Tests.tests
                , testGroup "Snap.Iteratee.Tests"
                            Snap.Iteratee.Tests.tests
                , testGroup "Snap.Util.GZip.Tests"
                            Snap.Util.GZip.Tests.tests
                , testGroup "Snap.Util.FileServe.Tests"
                            Snap.Util.FileServe.Tests.tests
                , testGroup "Snap.Util.FileUploads.Tests"
                            Snap.Util.FileUploads.Tests.tests
                , testGroup "Snap.Test.RequestBuilder.Tests"
                            Snap.Test.RequestBuilder.Tests.tests
                ]
