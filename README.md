# containers-backpack

[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/containers-backpack/blob/master/LICENSE)
[![Build status](https://secure.travis-ci.org/kowainik/containers-backpack.svg)](https://travis-ci.org/kowainik/containers-backpack)

Structure of this repository:

* `containers-sig-readonly`: signatures for read-only maps
* `containers-sig`: signatures for maps that can be modified
* `containers-ordered-strict`: implementation of signatures for `Map.Srict` data type from `containers` package
* `containers-int-strict`: implementation of signatures for `IntMap.Strict` type from `containers` package
* `containers-unordered-strict`: implementation of signatures for `HashMap.Strict` from `unordered-containers` package
* `containers-primitive`: implementation of signatures for `Map.Lifted.Lifted` from `primitive-containers` package
* `containers-contrib-readonly`: general functions for maps implemented using `containers-sig-readonly` package
* `containers-contrib`: general functions for maps implemented using `containers-sig-readonly` and `containers-sig` packages
* `containers-example`: package that mixes signatures and different implementations
