# Change Log / Release Notes

## unknown

### Enhancements

  * Added custom cursor shapes for resizing and moving windows.

### Bug Fixes

  * Fixed border color of windows with alpha channel. Now all windows have the
    same opaque border color.

## 0.17.0 (October 27, 2021)

### Enhancements

  * Migrated `X.L.LayoutCombinators.(|||)` into `XMonad.Layout`, providing the
    ability to directly jump to a layout with the `JumpToLayout` message.

  * Recompilation now detects `stack.yaml` (can be a symlink) alongside
    `xmonad.hs` and switches to using `stack ghc`. We also updated INSTALL.md
    with instructions for cabal-install that lead to correct recompilation.

    Deprecation warnings during recompilation are no longer suppressed to make
    it easier for us to clean up the codebase. These can still be suppressed
    manually using an `OPTIONS_GHC` pragma with `-Wno-deprecations`.

  * Improve handling of XDG directories.

      1. If all three of xmonad's environment variables (`XMONAD_DATA_DIR,`
         `XMONAD_CONFIG_DIR`, and `XMONAD_CACHE_DIR`) are set, use them.
      2. If there is a build script called `build` (see [these build scripts]
         for usage examples) or configuration `xmonad.hs` in `~/.xmonad`, set
         all three directories to `~/.xmonad`.
      3. Otherwise, use the `xmonad` directory in `XDG_DATA_HOME`,
         `XDG_CONFIG_HOME`, and `XDG_CACHE_HOME` (or their respective
         fallbacks). These directories are created if necessary.

    In the cases of 1. and 3., the build script or executable is expected to be
    in the config dir.

    Additionally, the xmonad config binary and intermediate object files were
    moved to the cache directory (only relevant if using XDG or
    `XMONAD_CACHE_DIR`).

  * Added `Foldable`, `Functor`, and `Traversable` instances for `Stack`.

  * Added `Typeable layout` constraint to `LayoutClass`, making it possible to
    cast `Layout` back into a concrete type and extract current layout state
    from it.

  * Export constructor for `Choose` and `CLR` from `Module.Layout` to allow
    pattern-matching on the left and right sub-layouts of `Choose l r a`.

  * Added `withUnfocused` function to `XMonad.Operations`, allowing for `X`
    operations to be applied to unfocused windows.

  * Added `willFloat` function to `XMonad.ManageHooks` to detect whether the
    (about to be) managed window will be a floating window or not

[these build scripts]: https://github.com/xmonad/xmonad-testing/tree/master/build-scripts

### Bug Fixes

  * Fixed a bug when using multiple screens with different dimensions, causing
    some floating windows to be smaller/larger than the size they requested.

  * Compatibility with GHC 9.0

  * Fixed dunst notifications being obscured when moving floats.
    https://github.com/xmonad/xmonad/issues/208

### Breaking Changes

  * Made `(<&&>)` and `(<||>)` non-strict in their right operand; i.e., these
    operators now implement short-circuit evaluation so the right operand is
    evaluated only if the left operand does not suffice to determine the
    result.

  * Change `ScreenDetail` to a newtype and make `RationalRect` strict in its
    contents.

  * Added the `extensibleConf` field to `XConfig` which makes it easier for
    contrib modules to have composable configuration (custom hooks, …).

  * `util/GenerateManpage.hs` is no longer distributed in the tarball.
    Instead, the manpage source is regenerated and manpage rebuilt
    automatically in CI.

  * `DestroyWindowEvent` is now broadcasted to layouts to let them know
    window-specific resources can be discarded.

## 0.15 (September 30, 2018)

  * Reimplement `sendMessage` to deal properly with windowset changes made
    during handling.

  * Add new library functions `windowBracket` and `modifyWindowSet` to
    `XMonad.Operations`.

## 0.14.2 (August 21, 2018)

### Bug Fixes

  * Add the sample configuration file xmonad.hs again to the release tarball.
    [https://github.com/xmonad/xmonad/issues/181]

## 0.14.1 (August 20, 2018)

### Breaking Changes

  * The cabal build no longer installs xmonad.hs, xmonad.1, and xmonad.1.html
    as data files. The location cabal picks for chose files isn't useful as
    standard tools like man(1) won't find them there. Instead, we rely on
    distributors to pick up the files from the source tarball during the build
    and to install them into proper locations where their users expect them.
    [https://github.com/xmonad/xmonad/pull/127]

### Bug Fixes

  * Add support for GHC 8.6.x by providing an instance for 'MonadFail X'. A
    side effect of that change is that our code no longer compiles with GHC
    versions prior to 8.0.x. We could work around that, no doubt, but the
    resulting code would require CPP and Cabal flags and whatnot. It feels more
    reasonable to just require a moderately recent compiler instead of going
    through all that trouble.

  * xmonad no longer always recompile on startup. Now it only does so if the
    executable does not have the name that would be used for the compilation
    output. The purpose of recompiling and executing the results in this case is
    so that the `xmonad` executable in the package can be used with custom
    configurations.

### Enhancements

  * Whenever xmonad recompiles, it now explains how it is attempting to
    recompile, by outputting logs to stderr. If you are using xmonad as a custom
    X session, then this will end up in a `.xsession-errors` file.

## 0.14 (July 30, 2018)

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

  * Compiles with GHC 8.4.1

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
