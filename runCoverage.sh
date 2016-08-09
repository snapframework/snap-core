#!/bin/sh

set -e

PKGVERSION=$(cabal info . | awk '{print $2;exit}')

echo $PKGVERSION

ROOT=dist-newstyle/build/$PKGVERSION
DIR=$ROOT/hpc/vanilla
HPCDIR=$DIR/mix/testsuite

DESTDIR=hpc

EXCLUDES='Main
Snap.Core.Tests
Snap.Internal.Debug
Snap.Internal.Http.Parser.Tests
Snap.Internal.Http.Server.Tests
Snap.Internal.Http.Types.Tests
Snap.Internal.Parsing.Tests
Snap.Internal.Routing.Tests
Snap.Internal.Test.Assertions
Snap.Internal.Test.RequestBuilder
Snap.Test
Snap.Test.Common
Snap.Test.Tests
Snap.Types.Tests
Snap.Types.Headers.Tests
Snap.Util.FileServe.Tests
Snap.Util.FileUploads.Tests
Snap.Util.GZip.Tests
Snap.Util.Proxy.Tests
Snap.Util.Readable.Tests
Text.Snap.Templates.Tests'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

rm -Rf $DESTDIR
mkdir -p $DESTDIR
ls -al
find . -name "*.tix"
echo hpc markup --hpcdir=$HPCDIR --destdir=$DESTDIR testsuite
hpc markup $EXCL --hpcdir=$HPCDIR --destdir=$DESTDIR testsuite # >/dev/null 2>&1

cat <<EOF

Test coverage report written to $DESTDIR.
EOF
