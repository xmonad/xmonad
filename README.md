<p align="center">
  <a href="https://xmonad.org/"><img alt="XMonad logo" src="https://xmonad.org/images/logo-wrapped.svg" height=150></a>
</p>
<p align="center">
  <a href="https://hackage.haskell.org/package/xmonad"><img alt="Hackage" src="https://img.shields.io/hackage/v/xmonad?logo=haskell"></a>
  <a href="https://github.com/xmonad/xmonad/blob/readme/LICENSE"><img alt="License" src="https://img.shields.io/github/license/xmonad/xmonad"></a>
  <a href="https://haskell.org/"><img alt="Made in Haskell" src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell"></a>
  <br>
  <a href="https://github.com/xmonad/xmonad/actions/workflows/stack.yml"><img alt="Stack" src="https://img.shields.io/github/workflow/status/xmonad/xmonad/Stack?label=Stack&logo=githubactions&logoColor=white"></a>
  <a href="https://github.com/xmonad/xmonad/actions/workflows/haskell-ci.yml"><img alt="Cabal" src="https://img.shields.io/github/workflow/status/xmonad/xmonad/Haskell-CI?label=Cabal&logo=githubactions&logoColor=white"></a>
  <a href="https://github.com/xmonad/xmonad/actions/workflows/nix.yml"><img alt="Nix" src="https://img.shields.io/github/workflow/status/xmonad/xmonad/Nix?label=Nix&logo=githubactions&logoColor=white"></a>
  <br>
  <a href="https://github.com/sponsors/xmonad"><img alt="GitHub Sponsors" src="https://img.shields.io/github/sponsors/xmonad?label=GitHub%20Sponsors&logo=githubsponsors"></a>
  <a href="https://opencollective.com/xmonad"><img alt="Open Collective" src="https://img.shields.io/opencollective/all/xmonad?label=Open%20Collective&logo=opencollective"></a>
  <br>
  <a href="https://web.libera.chat/#xmonad"><img alt="Chat on #xmonad@irc.libera.chat" src="https://img.shields.io/badge/%23%20chat-on%20libera-brightgreen"></a>
  <a href="https://matrix.to/#/#xmonad:matrix.org"><img alt="Chat on #xmonad:matrix.org" src="https://img.shields.io/matrix/xmonad:matrix.org?logo=matrix"></a>
</p>

# xmonad

**A tiling window manager for X11.**

[XMonad][web:xmonad] is a tiling window manager for X11. Windows are arranged
automatically to tile the screen without gaps or overlap, maximising
screen use. Window manager features are accessible from the keyboard:
a mouse is optional. xmonad is written, configured and extensible in
Haskell. Custom layout algorithms, key bindings and other extensions
may be written by the user in config files. Layouts are applied
dynamically, and different layouts may be used on each
workspace. Xinerama is fully supported, allowing windows to be tiled
on several physical screens.

This repository contains the [xmonad][hackage:xmonad] package, a minimal,
stable, yet extensible core. It is accompanied by
[xmonad-contrib][gh:xmonad-contrib], a library of hundreds of additional
community-maintained tiling algorithms and extension modules. The two combined
make for a powerful X11 window-manager with endless customization
possibilities. They are, quite literally, libraries for creating your own
window manager.

## Installation

For installation and configuration instructions, please see:

 * [downloading and installing xmonad][web:download]
 * [installing latest xmonad snapshot from git][web:install]
 * [configuring xmonad][web:tutorial]

If you run into any trouble, consult our [documentation][web:documentation] or
ask the [community][web:community] for help.

## Contributing

We welcome all forms of contributions:

 * [bug reports and feature ideas][gh:xmonad:issues]
   (also to [xmonad-contrib][gh:xmonad-contrib:issues])
 * [bug fixes, new features, new extensions][gh:xmonad:pulls]
   (usually to [xmonad-contrib][gh:xmonad-contrib:pulls])
 * documentation fixes and improvements: [xmonad][gh:xmonad],
   [xmonad-contrib][gh:xmonad-contrib], [xmonad-web][gh:xmonad-web]
 * helping others in the [community][web:community]
 * financial support: [GitHub Sponsors][gh:xmonad:sponsors],
   [Open Collective][opencollective:xmonad]

Please do read the [CONTRIBUTING][gh:xmonad:contributing] document for more
information about bug reporting and code contributions. For a brief overview
of the architecture and code conventions, see the [documentation for the
`XMonad.Doc.Developing` module][doc:developing]. If in doubt, [talk to
us][web:community].

## Authors

Started in 2007 by [Spencer Janssen][gh:spencerjanssen], [Don
Stewart][gh:donsbot] and [Jason Creighton][gh:JasonCreighton], the
[XMonad][web:xmonad] project lives on thanks to [new generations of
maintainers][gh:xmonad:maintainers] and [dozens of
contributors][gh:xmonad:contributors].

[gh:spencerjanssen]: https://github.com/spencerjanssen
[gh:donsbot]: https://github.com/donsbot
[gh:JasonCreighton]: https://github.com/JasonCreighton

[doc:developing]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html
[gh:xmonad-contrib:issues]: https://github.com/xmonad/xmonad-contrib/issues
[gh:xmonad-contrib:pulls]: https://github.com/xmonad/xmonad-contrib/pulls
[gh:xmonad-contrib]: https://github.com/xmonad/xmonad-contrib
[gh:xmonad-web]: https://github.com/xmonad/xmonad-web
[gh:xmonad:contributing]: https://github.com/xmonad/xmonad/blob/master/CONTRIBUTING.md
[gh:xmonad:contributors]: https://github.com/xmonad/xmonad/graphs/contributors
[gh:xmonad:issues]: https://github.com/xmonad/xmonad/issues
[gh:xmonad:maintainers]: https://github.com/xmonad/xmonad/blob/master/MAINTAINERS.md
[gh:xmonad:pulls]: https://github.com/xmonad/xmonad/pulls
[gh:xmonad:sponsors]: https://github.com/sponsors/xmonad
[gh:xmonad]: https://github.com/xmonad/xmonad
[hackage:xmonad]: https://hackage.haskell.org/package/xmonad
[opencollective:xmonad]: https://opencollective.com/xmonad
[web:community]: https://xmonad.org/community.html
[web:documentation]: https://xmonad.org/documentation.html
[web:download]: https://xmonad.org/download.html
[web:install]: https://xmonad.org/INSTALL.html
[web:tutorial]: https://xmonad.org/TUTORIAL.html
[web:xmonad]: https://xmonad.org/
