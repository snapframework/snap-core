#!/bin/sh

set -e

export LC_ALL=C
export LANG=C

rm -f testsuite.tix

# TODO How do we find the executable without knowing the version number in dist-newstyle?
./dist-newstyle/build/snap-core-1.0.0.0/build/testsuite/testsuite -j4 -a1000 $*

DIR="./dist-newstyle/hpc"

rm -Rf $DIR
mkdir -p $DIR
mkdir -p out

# NOTE
# Snap.Internal.Util.FileUploads shouldn't be in the excludes list. This is a
# temporary workaround so we can release.

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
Snap.Internal.Util.FileUploads
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

hpc markup $EXCL --destdir=$DIR testsuite

rm -f testsuite.tix

#TODO only copy hpc results if this script is called from deploy_hpc.sh
cp -r $DIR out/

cat <<EOF

Test coverage report written to $DIR.
EOF
