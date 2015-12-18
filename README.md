# xmonad: A Tiling Window Manager

[xmonad][] is a tiling window manager for X. Windows are arranged
automatically to tile the screen without gaps or overlap, maximising
screen use. Window manager features are accessible from the keyboard:
a mouse is optional. xmonad is written, configured and extensible in
Haskell. Custom layout algorithms, key bindings and other extensions
may be written by the user in config files. Layouts are applied
dynamically, and different layouts may be used on each
workspace. Xinerama is fully supported, allowing windows to be tiled
on several physical screens.

## Quick Start

  * From hackage:

        cabal update
        cabal install xmonad xmonad-contrib

  * Alternatively, build from source using the following repositories:

    - <https://github.com/xmonad/xmonad>

    - <https://github.com/xmonad/xmonad-contrib>

For the full story, read on.

## Building

Building is quite straightforward, and requires a basic Haskell toolchain.
On many systems xmonad is available as a binary package in your
package system (e.g. on Debian or Gentoo). If at all possible, use this
in preference to a source build, as the dependency resolution will be
simpler.

We'll now walk through the complete list of toolchain dependencies.

  * GHC: the Glasgow Haskell Compiler

    You first need a Haskell compiler. Your distribution's package
    system will have binaries of GHC (the Glasgow Haskell Compiler),
    the compiler we use, so install that first. If your operating
    system's package system doesn't provide a binary version of GHC
    and the `cabal-install` tool, you can install both using the
    [Haskell Platform][platform].

    It shouldn't be necessary to compile GHC from source -- every common
    system has a pre-build binary version.  However, if you want to
    build from source, the following links will be helpful:

      - GHC: <http://haskell.org/ghc/>

      - Cabal: <http://haskell.org/cabal/download.html>

  * X11 libraries:

    Since you're building an X application, you'll need the C X11
    library headers. On many platforms, these come pre-installed. For
    others, such as Debian, you can get them from your package manager:

        $ apt-get install libx11-dev libxinerama-dev libxext-dev

## Running xmonad

Add:

    exec $HOME/.cabal/bin/xmonad

to the last line of your `.xsession` or `.xinitrc` file.

## Configuring

See the `CONFIG` document.

## XMonadContrib

There are many extensions to xmonad available in the XMonadContrib
(xmc) library. Examples include an ion3-like tabbed layout, a
prompt/program launcher, and various other useful modules.
XMonadContrib is available at:

  * Latest release: <http://hackage.haskell.org/package/xmonad-contrib>

  * Git version: <https://github.com/xmonad/xmonad-contrib>

## Other Useful Programs

A nicer xterm replacement, that supports resizing better:

  * urxvt: <http://software.schmorp.de/pkg/rxvt-unicode.html>

For custom status bars:

  * xmobar: <http://hackage.haskell.org/package/xmobar>

  * taffybar: <https://github.com/travitch/taffybar>

  * dzen: <http://gotmor.googlepages.com/dzen>

For a program dispatch menu:

  * [XMonad.Prompt.Shell][xmc-prompt-shell]: (from [XMonadContrib][])

  * dmenu: <http://www.suckless.org/download/>

  * gmrun: (in your package system)

## Authors

  * Spencer Janssen
  * Don Stewart
  * Jason Creighton

[xmonad]: http://xmonad.org
[xmonadcontrib]: https://hackage.haskell.org/package/xmonad-contrib
[xmc-prompt-shell]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Prompt-Shell.html
[platform]: http://haskell.org/platform/
