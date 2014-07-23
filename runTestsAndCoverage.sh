#!/bin/sh

set -e

if [ -z "$DEBUG" ]; then
    export DEBUG="testsuite"
fi

SUITE=./dist/build/testsuite/testsuite

export LC_ALL=C
export LANG=C

rm -f testsuite.tix

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal configure --enable-tests
then
    cabal build
EOF
    exit;
fi

./dist/build/testsuite/testsuite -j4 -a1000 $*

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
Snap.Core.Tests
Snap.Internal.Debug
Snap.Internal.Http.Parser.Tests
Snap.Internal.Http.Server.Tests
Snap.Internal.Http.Types.Tests
Snap.Internal.Parsing.Tests
Snap.Internal.Routing.Tests
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

hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $DIR.
EOF
