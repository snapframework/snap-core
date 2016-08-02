Snap Framework Core
===================

[![Build Status](https://travis-ci.org/snapframework/snap-core.svg?branch=master)](https://travis-ci.org/snapframework/snap-core)

Snap is a web framework for Haskell. For more information about Snap, read the
`README.SNAP.md` or visit the Snap project website at
http://www.snapframework.com/.

## Library contents

This is the `snap-core` library, which contains:

  * primitive types and functions for HTTP (requests, responses, cookies,
    post/query parameters, etc).

  * a "Snap" monad interface for programming web handlers, which allows:

    * stateful access to the HTTP request and response objects.

    * monadic failure (i.e. MonadPlus/Alternative instances) for declining to
      handle requests and chaining handlers together.

    * early termination of the computation if you know early what you want to
      return and want to prevent further monadic processing.

  * useful utilities for web handlers, like file serving and gzip/zlib
    compression.


Building snap-core
===================

The snap-core library is built using [Cabal](http://www.haskell.org/cabal/) and
[Hackage](http://hackage.haskell.org/packages/hackage.html). Just run

    cabal install

from the `snap-core` toplevel directory.


## Building the Haddock Documentation

The haddock documentation can be built using the supplied `haddock.sh` shell
script:

    ./haddock.sh

The docs get put in `dist/doc/html/`.


## Building the testsuite

Snap aims for 100% test coverage, and we're trying hard to stick to that.

To build the test suite, configure with --enable-tests:

    $ cabal configure --enable-tests
    $ cabal build

From here you can invoke the testsuite by running:

    $ ./runTestsAndCoverage.sh


The testsuite generates an `hpc` test coverage report in `dist/hpc`.
