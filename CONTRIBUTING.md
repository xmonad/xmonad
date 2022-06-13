# Contributing to xmonad and xmonad-contrib

## Before Creating a GitHub Issue

New issue submissions should adhere to the following guidelines:

  * Does your issue have to do with [xmonad][], [xmonad-contrib][], or
    maybe even with the [X11][] library?

    Please submit your issue to the **correct** GitHub repository.

  * To help you figure out which repository to submit your issue to,
    and to help us resolve the problem you are having, create the
    smallest configuration file you can that reproduces the problem.

    You may find that the [xmonad-testing][] repository is helpful in
    reproducing the problem with a smaller configuration file.

    Once you've done that please include the configuration file with
    your GitHub issue.

  * If possible, use the [xmonad-testing][] repository to test your
    configuration with the bleeding-edge development version of xmonad
    and xmonad-contrib.  We might have already fixed your problem.

## Contributing Changes/Patches

Have a change to xmonad that you want included in the next release?
Awesome!  Here are a few things to keep in mind:

  * Review the above section about creating GitHub issues.

  * It's always best to talk with the community before making any
    nontrivial changes to xmonad.  There are a couple of ways you can
    chat with us:

    - Join the [`#xmonad` IRC channel] on `irc.libera.chat` or the
      official [matrix channel], which is linked to IRC.  This is the
      preferred (and fastest!) way to get into contact with us.

    - Post a message to the [mailing list][ml].

  * [XMonad.Doc.Developing][xmonad-doc-developing] is a great
    resource to get an overview of xmonad.  Make sure to also check
    it if you want more details on the coding style.

  * Continue reading this document!

## Expediting Reviews and Merges

Here are some tips for getting your changes merged into xmonad:

  * If your changes can go into [xmonad-contrib][] instead
    of [xmonad][], please do so.  We rarely accept new features to
    xmonad.  (Not that we don't accept changes to xmonad, just that we
    prefer changes to xmonad-contrib instead.)

  * Change the fewest files as possible.  If it makes sense, submit a
    completely new module to xmonad-contrib.

  * Your changes should include relevant entries in the `CHANGES.md`
    file.  Help us communicate changes to the community.

  * Make sure you test your changes against the most recent commit of
    [xmonad][] (and [xmonad-contrib][], if you're contributing there).
    If you're adding a new module or functionality, make sure to add an
    example in the documentation and in the PR description.

  * Make sure you run the automated tests.  Both [xmonad-contrib][]
    and [xmonad][] have test-suites that you could run with
    `stack test` for example.

  * When committing, try to follow existing practices.  For more
    information on what good commit messages look like, see [How to
    Write a Git Commit Message][commit-cbeams] and the [Kernel
    documentation][commit-kernel] about committing logical changes
    separately.

## Style Guidelines

Below are some common style guidelines that all of the core modules
follow.  Before submitting a pull request, make sure that your code does
as well!

  * Comment every top level function (particularly exported functions),
    and provide a type signature; use Haddock syntax in the comments.

  * Follow the coding style of the module that you are making changes to
    (`n` spaces for indentation, where to break long type signatures, …).

  * New code should not introduce any new warnings.  If you want to
    check this yourself before submitting a pull request, there is the
    `pedantic` flag, which is enforced in our CI.  You can enable it by
    building your changes with `stack build --flag xmonad:pedantic` or
    `cabal build --flag pedantic`.

  * Likewise, your code should be free of [hlint] warnings; this is also
    enforced in our GitHub CI.

  * Partial functions are to be avoided: the window manager should not
    crash, so do not call `error` or `undefined`.

  * Any pure function added to the core should have QuickCheck
    properties precisely defining its behavior.

  * New modules should identify the author, and be submitted under the
    same license as xmonad (BSD3 license).

## Keep rocking!

xmonad is a passion project created and maintained by the community.
We'd love for you to maintain your own contributed modules (approve
changes from other contributors, review code, etc.).  However, before
we'd be comfortable adding you to the [xmonad GitHub
organization][xmonad-gh-org] we need to trust that you have sufficient
knowledge of Haskell and git; and have a way of chatting with you ([IRC,
Matrix, etc.][community]).

[hlint]: https://github.com/ndmitchell/hlint
[xmonad]: https://github.com/xmonad/xmonad
[xmonad-contrib]: https://github.com/xmonad/xmonad-contrib
[xmonad-testing]: https://github.com/xmonad/xmonad-testing
[x11]: https://github.com/xmonad/X11
[ml]: https://mail.haskell.org/cgi-bin/mailman/listinfo/xmonad
[xmonad-doc-developing]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html
[`#xmonad` IRC channel]: https://web.libera.chat/#xmonad
[matrix channel]: https://matrix.to/#/#xmonad:matrix.org
[commit-cbeams]: https://cbea.ms/git-commit/
[commit-kernel]: https://www.kernel.org/doc/html/v4.10/process/submitting-patches.html#separate-your-changes
[community]: https://xmonad.org/community.html
[xmonad-gh-org]: https://github.com/xmonad
