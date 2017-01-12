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

    - Post a message to the [mailing list][ml].

    - Join the `#xmonad` IRC channel on `chat.freenode.org`.

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

  * Make sure you test your changes using the [xmonad-testing][]
    repository.  Include a new configuration file that shows off your
    changes if possible by creating a PR on that repository as well.

[xmonad]: https://github.com/xmonad/xmonad
[xmonad-contrib]: https://github.com/xmonad/xmonad-contrib
[xmonad-testing]: https://github.com/xmonad/xmonad-testing
[x11]: https://github.com/xmonad/X11
[ml]: https://mail.haskell.org/cgi-bin/mailman/listinfo/xmonad
