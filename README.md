# containers-backpack

![backpack 1](https://user-images.githubusercontent.com/4276606/44077109-23f24b62-9fd5-11e8-8069-b6ca9e45db79.png)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/containers-backpack/blob/master/LICENSE)
[![Build status](https://secure.travis-ci.org/kowainik/containers-backpack.svg)](https://travis-ci.org/kowainik/containers-backpack)

> _You can't just carry everyone else's hopes and fears around in your backpack and expect to stand up straight._
>
> David Kirk

See detailed description in the blog post:

* [Picnic: put containers into a Backpack](https://kowainik.github.io/posts/2018-08-19-picnic-put-containers-into-a-backpack)

Structure of this repository:

* `containers-sig-readonly`: signatures for read-only maps
* `containers-sig`: signatures for maps that can be modified
* `containers-ordered-strict`: implementation of signatures for the [`Map.Strict`](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html) data type from the [`containers`](https://hackage.haskell.org/package/containers) package
* `containers-int-strict`: implementation of signatures for the [`IntMap.Strict`](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-IntMap-Strict.html) type from the [`containers`](https://hackage.haskell.org/package/containers) package
* `containers-unordered-strict`: implementation of signatures for the [`HashMap.Strict`](http://hackage.haskell.org/package/unordered-containers-0.2.9.0/docs/Data-HashMap-Strict.html) from the [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers) package
* `containers-primitive`: implementation of signatures for the [`Map.Lifted.Lifted`](http://hackage.haskell.org/package/primitive-containers-0.2.0/docs/Data-Map-Lifted-Lifted.html) from the [`primitive-containers`](http://hackage.haskell.org/package/primitive-containers) package
* `containers-contrib-readonly`: general functions for maps implemented using the `containers-sig-readonly` package
* `containers-contrib`: general functions for maps implemented using the `containers-sig-readonly` and `containers-sig` packages
* `containers-example`: package that mixes signatures and different implementations


### Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/) is licensed by [CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
