# Install XMonad

On many systems xmonad is available as a binary package in your
distribution (Debian, Ubuntu, Fedora, Arch, Gentoo, …).
It's by far the easiest way to get xmonad, although you'll miss out on the
latest features and fixes that may not have been released yet.

If you do want the latest and greatest, continue reading.
Those who install from distro can skip this and go straight to
[the XMonad Configuration Tutorial](TUTORIAL.md).

<!-- https://github.com/frnmst/md-toc -->
<!-- regenerate via: md_toc -s1 -p github INSTALL.md -->
<!--TOC-->

- [Dependencies](#dependencies)
- [Preparation](#preparation)
- [Download XMonad sources](#download-xmonad-sources)
- [Build XMonad](#build-xmonad)
  - [Build using Stack](#build-using-stack)
- [Make XMonad your window manager](#make-xmonad-your-window-manager)
- [Don't Recompile on Every Startup](#dont-recompile-on-every-startup)

<!--TOC-->

## Dependencies

#### Debian, Ubuntu

``` shell
$ sudo apt install \
  git \
  libx11-dev libxft-dev libxinerama-dev libxrandr-dev libxss-dev
```

#### Fedora

``` shell
$ sudo dnf install \
  git \
  libX11-devel libXft-devel libXinerama-devel libXrandr-devel libXScrnSaver-devel
```

#### Arch

``` shell
$ sudo pacman -S \
  git \
  libx11 libxft libxinerama libxrandr libxss \
  pkgconf
```

## Preparation

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

Older versions of xmonad used `~/.xmonad` instead.
This is still supported, but XDG is preferred.

## Download XMonad sources

Still in `~/.config/xmonad`, clone `xmonad` and `xmonad-contrib` repositories
using [git][]:

``` shell
$ git clone https://github.com/xmonad/xmonad
$ git clone https://github.com/xmonad/xmonad-contrib
```

This will give you the latest `HEAD`; if you want you can also check
out a tagged release, e.g.:

``` shell
$ git clone --branch v0.15 https://github.com/xmonad/xmonad
$ git clone --branch v0.16 https://github.com/xmonad/xmonad-contrib
```

(Sources and binaries don't usually go into `~/.config`.  In our case,
however, it avoids complexities related to Haskell build tools and lets us
focus on the important bits of XMonad installation.)

## Build XMonad

There are two widely used Haskell build tools:

* [Stack][stack]
* [cabal-install][cabal-install]

We include instructions for both.
Unless you already know which one you prefer, use Stack, which is easier.

### Build using Stack

#### Install Stack

The easiest way to get [stack] is probably via your system's package
manager:

``` shell
$ sudo apt install haskell-stack    # Debian, Ubuntu
$ sudo dnf install stack            # Fedora
$ sudo pacman -S stack              # Arch
```

If you install stack via this method, it is advisable that you run
`stack upgrade` after installation.  This will make sure that you are on
the most recent version of the program, regardless of which version your
distribution actually packages.

If your distribution does not package stack, you can also easily install
it via the following command (this is the recommended way to install
stack via its [documentation][stack]):

``` shell
$ curl -sSL https://get.haskellstack.org/ | sh
```

Yet another way would be via [ghcup]; this is similar to installers like
`rustup`, in case you prefer that.

#### Create a New Project

Let's create a stack project.  Since we're already in the correct
directory (`~/.config/xmonad`) with `xmonad` and `xmonad-contrib`
subdirectories, starting a new stack project is as simple as running `stack
init`.

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

#### Install Everything

Installing things is as easy as typing `stack install`.  This will
install the correct version of GHC, as well as build all of the required
packages (`stack build`) and then copy the relevant executables
(`xmonad`, in our case) to `~/.local/bin`.  Make sure to add that
directory to your `$PATH`!

If you're getting build failures while building the `X11` package it may
be that you don't have the required C libraries installed.  See
[above](#dependencies).

#### Tell XMonad How to Recompile Itself

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

## Make XMonad your window manager

This step varies depending on your distribution and X display manager (if
any).

#### Debian, Ubuntu

`/etc/X11/xinit/xinitrc` runs `/etc/X11/Xsession` which runs `~/.xsession`, so
you probably want to put `exec xmonad` there (don't forget the shebang and chmod).

(Tested with `startx`, `xdm`, `lightdm`.)

By using `~/.xsession`, the distro takes care of stuff like dbus, ssh-agent, X
resources, etc.  If you want a completely manual X session, use `~/.xinitrc`
instead.  Or invoke `startx`/`xinit` with an explicit path.

Some newer display managers require an entry in `/usr/share/xsessions`.
To use your custom `~/.xsession`, put these lines to
`/usr/share/xsessions/default.desktop`:

```
[Desktop Entry]
Name=Default X session
Type=Application
Exec=default
```

(Tested with `sddm`.)

#### Fedora

`/etc/X11/xinit/xinitrc` runs `~/.Xclients`, so you probably want to put `exec
xmonad` there (don't forget the shebang and chmod).  Like in Debian, this can
be overridden by having a completely custom `~/.xinitrc` or passing arguments
to `startx`/`xinit`.

X display managers (e.g. `lightdm`) usually invoke `/etc/X11/xinit/Xsession`
instead, which additionally redirects output to `~/.xsession-errors` and also
tries `~/.xsession` before `~/.Xclients`.

Newer display managers require an entry in `/usr/share/xsessions`, which is
available in the `xorg-x11-xinit-session` package.

#### Arch

`/etc/X11/xinit/xinitrc` runs `twm`, `xclock` and 3 `xterm`s; users are
meant to just copy that to `~/.xinitrc` and
[customize](https://wiki.archlinux.org/title/Xinit#xinitrc) it: replace the
last few lines with `exec xmonad`.

Display managers like `lightdm` have their own `Xsession` script which invokes
`~/.xsession`.  Other display managers need an entry in
`/usr/share/xsessions`, https://aur.archlinux.org/packages/xinit-xsession/
provides one.

## Don't Recompile on Every Startup

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
[git]: https://git-scm.com/
[stack]: https://docs.haskellstack.org/en/stable/README/
[cabal-install]: https://www.haskell.org/cabal/
[ghcup]: https://www.haskell.org/ghcup/
[what xmonad would do]: https://github.com/xmonad/xmonad/blob/master/src/XMonad/Core.hs#L657-L665
