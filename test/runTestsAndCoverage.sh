#!/bin/sh

set -e

SUITE=./dist/build/testsuite/testsuite

rm -f testsuite.tix

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal configure -ftest
then
    cabal build
EOF
    exit;
fi

./dist/build/testsuite/testsuite -j4 -a1000

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
Data.CIByteString
Snap.Internal.Http.Parser.Tests
Snap.Internal.Http.Server.Tests
Snap.Internal.Http.Types.Tests
Snap.Internal.Iteratee.Tests
Text.Snap.Templates.Tests
Snap.Test.Common'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $DIR.
EOF
