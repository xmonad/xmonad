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
  - [Build using cabal-install](#build-using-cabal-install)
- [Make XMonad your window manager](#make-xmonad-your-window-manager)
- [Custom Build Script](#custom-build-script)

<!--TOC-->

## Dependencies

#### Debian, Ubuntu

``` console
$ sudo apt install \
> git \
> libx11-dev libxft-dev libxinerama-dev libxrandr-dev libxss-dev
```

#### Fedora

``` console
$ sudo dnf install \
> git \
> libX11-devel libXft-devel libXinerama-devel libXrandr-devel libXScrnSaver-devel
```

#### Arch

``` console
$ sudo pacman -S \
> git \
> xorg-server xorg-apps xorg-xinit xorg-xmessage \
> libx11 libxft libxinerama libxrandr libxss \
> pkgconf
```

#### Void

``` console
$ sudo xbps-install \
> git \
> ncurses-libtinfo-libs ncurses-libtinfo-devel \
> libX11-devel libXft-devel libXinerama-devel libXrandr-devel libXScrnSaver-devel \
> pkg-config
```

## Preparation

We'll use the [XDG] directory specifications here, meaning our
configuration will reside within `$XDG_CONFIG_HOME`, which is
`~/.config` on most systems.  Let's create this directory and move to
it:

``` console
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

``` console
$ git clone https://github.com/xmonad/xmonad
$ git clone https://github.com/xmonad/xmonad-contrib
```

This will give you the latest `HEAD`; if you want you can also check
out a tagged release, e.g.:

``` console
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

``` console
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

``` console
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

``` console
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

``` console
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

With `stack.yaml` alongside `xmonad.hs`, xmonad now knows that it needs to use
`stack ghc` instead of just `ghc` when (re)compiling its configuration.
If you want to keep xmonad sources and the stack project elsewhere, but still
use `xmonad --recompile`, symlink your real `stack.yaml` into the xmonad
configuration directory, or [use a custom build script](#custom-build-script).

#### Install Everything

Installing things is as easy as typing `stack install`.  This will
install the correct version of GHC, as well as build all of the required
packages (`stack build`) and then copy the relevant executables
(`xmonad`, in our case) to `~/.local/bin`.  Make sure to add that
directory to your `$PATH`!  The command `which xmonad` should now return
that executable.  In case it does not, check if you still have xmonad
installed via your package manager and uninstall it.

If you're getting build failures while building the `X11` package it may
be that you don't have the required C libraries installed.  See
[above](#dependencies).

### Build using cabal-install

#### Install cabal-install

The easiest way to get [cabal-install] is probably via your system's package
manager:

``` console
$ sudo apt install cabal-install    # Debian, Ubuntu
$ sudo dnf install cabal-install    # Fedora
$ sudo pacman -S cabal-install      # Arch
```

If your distribution does not package cabal-install, [ghcup][] is another
option.  See also <https://www.haskell.org/cabal/#install-upgrade>.

#### Create a New Project

Let's create a cabal project.  Since we're already in the correct
directory (`~/.config/xmonad`) with `xmonad` and `xmonad-contrib`
subdirectories, we'll instruct cabal to use them.  Create a file named
`cabal.project` containing:

```
packages: */*.cabal
```

(If you skip this step, cabal will use the latest releases from [Hackage][]
instead.)

#### Install Everything

You'll need to update the cabal package index, build xmonad and xmonad-contrib
libraries and then build the xmonad binary:

``` console
$ cabal update
$ cabal install --package-env=$HOME/.config/xmonad --lib xmonad xmonad-contrib
$ cabal install --package-env=$HOME/.config/xmonad xmonad
```

This will create a GHC environment in `~/.config/xmonad` so that the libraries
are available for recompilation of the config file, and also install the
xmonad binary to `~/.cabal/bin/xmonad`.  Make sure you have that directory in
your `$PATH`!

If you're getting build failures while building the `X11` package it may
be that you don't have the required C libraries installed.  See
[above](#dependencies).

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
`/usr/share/xsessions`, <https://aur.archlinux.org/packages/xinit-xsession/>
provides one.

#### See also

* <https://xmonad.org/documentation.html#in-your-environment>
* [FAQ: How can I use xmonad with a display manager? (xdm, kdm, gdm)](https://wiki.haskell.org/Xmonad/Frequently_asked_questions#How_can_I_use_xmonad_with_a_display_manager.3F_.28xdm.2C_kdm.2C_gdm.29)

## Custom Build Script

If you need to customize what happens during `xmonad --recompile` (bound to
`M-q` by default), perhaps because your xmonad configuration is a whole
separate Haskell package, you need to create a so-called `build` file.  This
is quite literally just a shell script called `build` in your xmonad directory
(which is `~/.config/xmonad` for us) that tells xmonad how it should build its
executable.

A good starting point (this is essentially [what xmonad would do][]
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

Some example build scripts for `stack` and `cabal` are provided in the
`xmonad-contrib` distribution. You can see those online in the
[scripts/build][] directory. You might wish to use these if you have
special dependencies for your `xmonad.hs`, especially with cabal as
you must use a cabal file and often a `cabal.project` to specify them;
`cabal install --lib` above generally isn't enough, and when it is
it can be difficult to keep track of when you want to replicate your
configuration on another system.

#### Don't Recompile on Every Startup

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
exec $HOME/.cache/xmonad/xmonad-x86_64-linux
```

The `~/.cache` prefix is the `$XDG_CACHE_HOME` directory.  Note that
if your xmonad configuration resides within `~/.xmonad`, then the
executable will also be within that directory and not in
`$XDG_CACHE_HOME`.

[XDG]: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
[git]: https://git-scm.com/
[stack]: https://docs.haskellstack.org/en/stable/README/
[cabal-install]: https://www.haskell.org/cabal/
[ghcup]: https://www.haskell.org/ghcup/
[what xmonad would do]: https://github.com/xmonad/xmonad/blob/master/src/XMonad/Core.hs#L659-L667
[Hackage]: https://hackage.haskell.org/
[scripts/build]: https://github.com/xmonad/xmonad-contrib/blob/master/scripts/build
