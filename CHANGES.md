# Change Log / Release Notes

## 0.14 (Not Yet Released)

### Bug Fixes

  * The state file that xmonad uses while restarting itself is now
    removed after it is processed.  This fixes a bug that manifested
    in several different ways:

    - Names of old workspaces would be resurrected after a restart
    - Screen sizes would be wrong after changing monitor configuration (#90)
    - `spawnOnce` stopped working (xmonad/xmonad-contrib#155)
    - Focus did not follow when moving between workspaces (#87)
    - etc.

  * Recover old behavior (in 0.12) when `focusFollowsMouse == True`:
    the focus follows when the mouse enters another workspace
    but not moving into any window.

  * Compiles with GHC 8.4.0

  * Restored compatability with GHC version prior to 8.0.1 by removing the
    dependency on directory version 1.2.3.

## 0.13 (February 10, 2017)

### Breaking Changes

  * When restarting xmonad, resume state is no longer passed to the
    next process via the command line.  Instead, a temporary state
    file is created and xmonad's state is serialized to that file.

    When upgrading to 0.13 from a previous version, the `--resume`
    command line option will automatically migrate to a state file.

    This fixes issue #12.

### Enhancements

  * You can now control which directory xmonad uses for finding your
    configuration file and which one is used for storing the compiled
    version of your configuration.  In order of preference:

      1. New environment variables.  If you want to use these ensure
         you set the correct environment variable and also create the
         directory it references:

         - `XMONAD_CONFIG_DIR`
         - `XMONAD_CACHE_DIR`
         - `XMONAD_DATA_DIR`

      2. The `~/.xmonad` directory.

      3. XDG Base Directory Specification directories, if they exist:

         - `XDG_CONFIG_HOME/xmonad`
         - `XDG_CACHE_HOME/xmonad`
         - `XDG_DATA_HOME/xmonad`

    If none of these directories exist then one will be created using
    the following logic: If the relevant environment variable
    mentioned in step (1) above is set, the referent directory will be
    created and used.  Otherwise `~/.xmonad` will be created and used.

    This fixes a few issues, notably #7 and #56.

  * A custom build script can be used when xmonad is given the
    `--recompile` command line option.  If an executable named `build`
    exists in the xmonad configuration directory it will be called
    instead of `ghc`.  It takes one argument, the name of the
    executable binary it must produce.

    This fixes #8.  (One of two possible custom build solutions.  See
    the next entry for another solution.)

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

    Fixes #8.  (Second way to have a custom build environment for
    XMonad.  See previous entry for another solution.)

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
