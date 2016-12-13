# Change Log / Release Notes

## 0.13 (Sometime Early 2017)

### Breaking Changes

  * When restarting xmonad, resume state is no longer passed to the
    next process via the command line.  Instead, a temporary state
    file is created and xmonad's state is serialized to that file.

    When upgrading to 0.13 from a previous version, the `--resume`
    command line option will automatically migrate to a state file.

    This fixes issue #12.

### Enhancements

  * The XDG specification is now honored, with fallback support for
    `~/.xmonad`.  If the traditional `~/.xmonad` directory doesn't
    exist then the XDG configuration and data directories will be used
    instead.  Additionally, new environment variables can be used to
    override `~/.xmonad` and XDG:

      - `XMONAD_CONFIG_DIR`
      - `XMONAD_CACHE_DIR`
      - `XMONAD_DATA_DIR`

    This fixes a few issues, notably #7 and #56.

  * A custom build script can be used when xmonad is given the
    `--recompile` command line option.  If an executable named `build`
    exists in the xmonad configuration directory it will be called
    instead of `ghc`.  It takes one argument, the name of the
    executable binary it must produce.

    Note: the build script is called whenever `ghc` would have been
    called.  That means the `xmonad.hs` file (or any files in `lib`
    need to have newer time stamps than the generated executable in
    order for the build script to be called.

    This is one possible fix for issue #8.

  * For users who build their xmonad configuration using tools such as
    cabal or stack, there is another option for executing xmonad.

    Instead of running the `xmonad` executable directly, arrange to
    have your login manager run your configuration binary instead.
    Then, in your binary, use the new `launch` command instead of
    `xmonad`.

    This will keep xmonad from using its configuration file
    checking/compiling code and directly start the window manager
    without `exec`ing any other binary.

    See the documentation for the `launch` function in `XMonad.Main`
    for more details.

    This is another possible solution to issue #8.

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
