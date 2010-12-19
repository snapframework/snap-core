Snap Framework Core
===================

This is the Snap Framework Core library.  For more information about Snap, read
the `README.SNAP.md` or visit the Snap project website at
http://www.snapframework.com/.

Snap is a web framework for Haskell, based on iteratee I/O (as [popularized by
Oleg Kiselyov](http://okmij.org/ftp/Streams.html#iteratee)).


## Library contents

This is the `snap-core` library, which contains:

  * primitive types and functions for HTTP (requests, responses, cookies,
    post/query parameters, etc).

  * type aliases and helper functions for Iteratee I/O.

  * a "Snap" monad interface, inspired by
    [happstack's](http://happstack.com/index.html), for programming web
    handlers, which allows:

    * stateful access to the HTTP request and response objects.

    * monadic failure (i.e. MonadPlus/Alternative instances) for declining to
      handle requests and chaining handlers together.

    * early termination of the computation if you know early what you want to
      return and want to prevent further monadic processing.

  * Some useful utilities for web handlers, including gzip/zlib compression.


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

Snap-core has a fairly comprehensive testsuite. To build it, `cd` into the
`test/` directory and run

    $ cabal configure
    $ cabal build

From here you can invoke the testsuite by running:

    $ ./runTestsAndCoverage.sh 

The testsuite generates an `hpc` test coverage report in `test/dist/hpc`.
