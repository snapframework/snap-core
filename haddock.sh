#!/bin/sh

set -x

rm -Rf dist/doc

HADDOCK_OPTS='--html-location=http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html --css=extra/haddock.css'

cabal haddock $HADDOCK_OPTS --hyperlink-source $@

cp -r extra/fonts dist/doc/html/snap-core/
cp extra/logo.gif dist/doc/html/snap-core/haskell_icon.gif
cp extra/hscolour.css dist/doc/html/snap-core/src/
