structs
==========

[![Build Status](https://secure.travis-ci.org/ekmett/structs.png?branch=master)](http://travis-ci.org/ekmett/structs)

This package explores strict mutable data structures in Haskell. 

In particular, pointer-based data structures are effectively 'half price' due to the encoding used.

However, the result is that if you use the `slot` and `field` system wrong, you can and will `SEGFAULT`.

This means the `Internal` modules are very much internal.

Some documentation is available at
[http://ekmett.github.io/structs/Data-Struct.html](http://ekmett.github.io/structs/Data-Struct.html).

Building with stack
-------------------

If `stack` is your toolchain of choice, be aware that the `testsuite` depends
upon the `cabal_macros.h` file. The easiest way to make it available for CPP
inclusion is to use a symlink, example:

```
ln -s .stack-work/dist/x86_64-osx/Cabal-1.22.4.0 dist
```

Then you can `stack test` as you would normally do.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
