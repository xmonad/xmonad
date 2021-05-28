# Install XMonad

## Stack

### Preparation

We'll use the [XDG] directory specifications here, meaning our
configuration will reside within `$XDG_CONFIG_HOME`, which is
`~/.config` on most systems.  Let's create this directory and move to
it:

``` shell
  $ mkdir -p ~/.config/xmonad && cd ~/.config/xmonad
```

If you already have an `xmonad.hs` configuration, you can copy it over
now.  If not, you can use the defaults: create a file called `xmonad.hs`
with the following content:

``` haskell
  import XMonad

  main :: IO ()
  main = xmonad def
```

### Install Stack

The easiest way to get [stack] is probably via your system's package
manager.  For example, on Debian:

``` shell
  $ apt install haskell-stack
```

If your distribution does not package stack, you can also easily install
it via the following command (this is the recommended way to install
stack via its [documentation][stack]):

``` shell
  $ curl -sSL https://get.haskellstack.org/ | sh
```

Yet another way would be via [ghcup]; this is similar to installers like
`rustup`, in case you prefer that.

### Create a New Project

Let's create a stack project.  Since we're already in the correct
directory (`~/.config/xmonad`), we can start by cloning the `xmonad` and
the `xmonad-contrib` repositories:

``` shell
  $ git clone https://github.com/xmonad/xmonad
  $ git clone https://github.com/xmonad/xmonad-contrib
```

This will give you the latest `$HEAD`; if you want you can also check
out a tagged release, e.g.:

``` shell
  $ git clone --branch v0.16 https://github.com/xmonad/xmonad
  $ git clone --branch v0.17 https://github.com/xmonad/xmonad-contrib
```

Starting a new stack project is as simple as running `stack init`.
Stack should now inform you that it will use the relevant `stack` and
`cabal` files from `xmonad` and `xmonad-contrib` to generate its
`stack.yaml` file.  At the time of writing, this looks a little bit like
this:

```
  $ stack init
  Looking for .cabal or package.yaml files to use to init the project.
  Using cabal packages:
  - xmonad-contrib/
  - xmonad/

  Selecting the best among 19 snapshots...

  * Matches https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/17/9.yaml

  Selected resolver: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/17/9.yaml
  Initialising configuration using resolver: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/17/9.yaml
  Total number of user packages considered: 2
  Writing configuration to file: stack.yaml
  All done.
```

If you look into your current directory now, you should see a freshly
generated `stack.yaml` file:

```
  $ ls
  xmonad  xmonad-contrib  stack.yaml  xmonad.hs
```

The meat of that file (comments start with `#`, we've omitted them here)
will look a little bit like

``` yaml
  resolver:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/17/9.yaml

  packages:
  - xmonad
  - xmonad-contrib
```

### Install Everything

Installing things is as easy as typing `stack install`.  This will
install the correct version of GHC, as well as build all of the required
packages (`stack build`) and then copy the relevant executables
(`xmonad`, in our case) to `~/.local/bin`.  Make sure to add that
directory to your `$PATH`!

If you're getting build failures while building the `X11` package it may
be that you don't have the required C libraries installed.  Depending on
your system, this may be `libX11-devel`, or `libxss`.

### Tell XMonad How to Recompile Itself

In order to tell xmonad to invoke `stack build` when we issue `xmonad
--recompile` (bound to `M-q` by default), we need to create a so-called
`build` file.  This is quite literally just a shell script called
`build` in your xmonad directory (which is `~/.config/xmonad` for us)
that tells xmonad how it should build its executable.

A good starting point (this is essentially [what xmonad would do]
without a build file, with the exception that we are invoking `stack
ghc` instead of plain `ghc`) would be

``` shell
  #!/bin/sh

  exec stack ghc --  \
    --make xmonad.hs \
    -i               \
    -ilib            \
    -fforce-recomp   \
    -main-is main    \
    -v0              \
    -o "$1"
```

Don't forget to mark the file as `+x`: `chmod +x build`!

And that's it!  Recompilation should work normally now, though you will
potentially need to restart your computer, or at least the running X
session, first.

### Don't Recompile on Every Startup

By default, xmonad always recompiles itself when a build script is used
(because the build script could contain arbitrary code, so a simple
check whether the `xmonad.hs` file changed is not enough).  If you find
that too annoying, then you can use the `xmonad-ARCH` executable that
`xmonad --recompile` generates instead of `xmonad` in your startup.  For
example, instead of writing

``` shell
  exec xmonad
```

in your `~/.xinitrc`, you would write

``` shell
  exec $HOME/.local/share/xmonad/xmonad-x86_64-linux
```

The `~/.local/share` prefix is the `$XDG_DATA_DIR` directory.  Note that
if your xmonad configuration resides within `~/.xmonad`, then the
executable will also be within that directory and not in
`$XDG_DATA_DIR`.

[XDG]: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
[stack]: https://docs.haskellstack.org/en/stable/README/
[ghcup]: https://www.haskell.org/ghcup/
[what xmonad would do]: https://github.com/xmonad/xmonad/blob/master/src/XMonad/Core.hs#L657-L665
