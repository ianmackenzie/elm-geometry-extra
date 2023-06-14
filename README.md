# elm-geometry-extra

This package contains extra community-contributed functionality for [`elm-geometry`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/).
Functionality available in this package may be more specialized, more experimental or less fully
tested than in `elm-geometry`, but that also means it contains lots of useful functionality not
present in `elm-geometry` itself!

## Using this package

Install `elm-geometry` and `elm-geometry-extra`:

```
elm install ianmackenzie/elm-geometry
elm install ianmackenzie/elm-geometry-extra
```

Then, to use (for example) `BoundingBox2d` related functions from both `elm-geometry` and
`elm-geometry-extra`, in your Elm code you could do either

```elm
import BoundingBox2d 
import BoundingBox2d.Extra
```

or

```elm
import BoundingBox2d 
import BoundingBox2d.Extra as BoundingBox2d
```

to 'merge' the modules together (so the rest of your code can be written as if all functions were
part of a single `BoundingBox2d` module).

## Contributing

Yes please! `elm-geometry-extra` is a great place to contribute larger or more experimental new 
features for `elm-geometry`. In general, a function that might add to (for example) the
`BoundingBox2d` module in `elm-geometry` would go in a `BoundingBox2d.Extra` module in
`elm-geometry-extra`.

If at all possible, please also add some tests for your new functionality, using the existing tests
as an example.
