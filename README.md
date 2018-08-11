# containers-backpack

[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/containers-backpack/blob/master/LICENSE)
[![Build status](https://secure.travis-ci.org/kowainik/containers-backpack.svg)](https://travis-ci.org/kowainik/containers-backpack)

Structure of this repository:

* `containers-sig-readonly`: signatures for read-only maps
* `containers-sig`: signatures for maps that can be modified
* `containers-ordered-strict`: implementation of signatures for [`Map.Strict`](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html) data type from [`containers`](https://hackage.haskell.org/package/containers) package
* `containers-int-strict`: implementation of signatures for [`IntMap.Strict`](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-IntMap-Strict.html) type from [`containers`](https://hackage.haskell.org/package/containers) package
* `containers-unordered-strict`: implementation of signatures for [`HashMap.Strict`](http://hackage.haskell.org/package/unordered-containers-0.2.9.0/docs/Data-HashMap-Strict.html) from [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers) package
* `containers-primitive`: implementation of signatures for [`Map.Lifted.Lifted`](http://hackage.haskell.org/package/primitive-containers-0.2.0/docs/Data-Map-Lifted-Lifted.html) from [`primitive-containers`](http://hackage.haskell.org/package/primitive-containers) package
* `containers-contrib-readonly`: general functions for maps implemented using `containers-sig-readonly` package
* `containers-contrib`: general functions for maps implemented using `containers-sig-readonly` and `containers-sig` packages
* `containers-example`: package that mixes signatures and different implementations
