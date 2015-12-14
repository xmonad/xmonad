# Change Log / Release Notes

## 0.12 (December 14, 2015)

  * Compiles with GHC 7.10.2, 7.8.4, and 7.6.3

  * Use of [data-default][] allows using `def` where previously you
    had to write `defaultConfig`, `defaultXPConfig`, etc.

  * The [setlocale][] package is now used instead of a binding shipped
    with xmonad proper allowing the use of `Main.hs` instead of
    `Main.hsc`

  * No longer encodes paths for `spawnPID`

  * The default `manageHook` no longer floats Gimp windows

  * Doesn't crash when there are fewer workspaces than screens

  * `Query` is now an instance of `Applicative`

  * Various improvements to the example configuration file

[data-default]: http://hackage.haskell.org/package/data-default
[setlocale]: https://hackage.haskell.org/package/setlocale
