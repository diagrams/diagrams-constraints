_diagrams-constraints_ is a backend for [diagrams] that implements constraint solving.
[diagrams]: http://projects.haskell.org/diagrams/

# Installation

cabal sandbox add-source the no-data branch of diagrams-core ad the generalize-double branches of diagrams-lib and diagrams-svg.

Run
```
cabal install
```

This should download the rest of the Haskell dependencies.

To actually run it, you will need to install [Z3] as well (latest nightly a.k.a. "4.3.2", accessible through the "Planned" releases), and point
sbv to it by setting the SBV_Z3 environment variable to the path of the executable.
[Z3]: http://z3.codeplex.com/

So far, the code has only been tested on Windows.

# Usage

```
cabal repl
:load CMears
:main --output test.svg
```

Pull requests welcome.