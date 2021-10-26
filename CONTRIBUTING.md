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

  * Make sure you read the section on rebasing and squashing commits
    below.

## Style Guidelines

Below are some common style guidelines that all of the core modules
follow.  Before submitting a pull request, make sure that your code does
as well!

  * Comment every top level function (particularly exported functions),
    and provide a type signature; use Haddock syntax in the comments.

  * Follow the coding style of the module that you are making changes to
    (`n` spaces for indentation, where to break long type signatures, …)

  * New code should not introduce any new warnings.  If you want to
    check this yourself before submitting a pull request, there is the
    `pedantic` flag, which is enforced in our CI.  You can enable it by
    building your changes with `stack build --flag xmonad:pedantic` or
    `cabal build --flag pedantic`.

  * Likewise, your code should be free of [hlint] warnings; this is also
    enforced in our GitHub CI.

  * Partial functions are to be avoided: the window manager should not
    crash, so do not call `error` or `undefined`

  * Any pure function added to the core should have QuickCheck
    properties precisely defining its behavior.

  * New modules should identify the author, and be submitted under the
    same license as xmonad (BSD3 license).

## Rebasing and Squashing Commits

Under no circumstances should you ever merge the master branch into
your feature branch.  This makes it nearly impossible to review your
changes and we *will not accept your PR* if you do this.

Instead of merging you should rebase your changes on top of the master
branch.  If a core team member asks you to "rebase your changes" this
is what they are talking about.

It's also helpful to squash all of your commits so that your pull
request only contains a single commit.  Again, this makes it easier to
review your changes and identify the changes later on in the Git
history.

### How to Rebase Your Changes

The goal of rebasing is to bring recent changes from the master branch
into your feature branch.  This often helps resolve conflicts where
you have changed a file that also changed in a recently merged pull
request (i.e. the `CHANGES.md` file).  Here is how you do that.

  1. Make sure that you have a `git remote` configured for the main
     repository.  I like to call this remote `upstream`:
     ```shell
     $ git remote add upstream https://github.com/xmonad/xmonad-contrib.git
     ```

  2. Pull from upstream and rewrite your changes on top of master.  For
     this to work you should not have any modified files in your
     working directory.  Run these commands from within your feature
     branch (the branch you are asking to be merged):

     ```shell
     $ git fetch --all
     $ git pull --rebase upstream master
     ```

  3. If the rebase was successful you can now push your feature branch
     back to GitHub.  You need to force the push since your commits
     have been rewritten and have new IDs:

     ```shell
     $ git push --force-with-lease
     ```

  4. Your pull request should now be conflict-free and only contain the
     changes that you actually made.

### How to Squash Commits

The goal of squashing commits is to produce a clean Git history where
each pull request contains just one commit.

  1. Use `git log` to see how many commits you are including in your
     pull request.  (If you've already submitted your pull request you
     can see this in the GitHub interface.)

  2. Rebase all of those commits into a single commit.  Assuming you
     want to squash the last four (4) commits into a single commit:
     ```shell
     $ git rebase -i HEAD~4
     ```

  3. Git will open your editor and display the commits you are
     rebasing with the word "pick" in front of them.

  4. Leave the first listed commit as "pick" and change the remaining
     commits from "pick" to "squash".

  5. Save the file and exit your editor.  Git will create a new commit
     and open your editor so you can modify the commit message.

  6. If everything was successful you can push your changed history
     back up to GitHub:
     ```shell
     $ git push --force-with-lease
     ```

[hlint]: https://github.com/ndmitchell/hlint
[xmonad]: https://github.com/xmonad/xmonad
[xmonad-contrib]: https://github.com/xmonad/xmonad-contrib
[xmonad-testing]: https://github.com/xmonad/xmonad-testing
[x11]: https://github.com/xmonad/X11
[ml]: https://mail.haskell.org/cgi-bin/mailman/listinfo/xmonad
[xmonad-doc-developing]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html
[`#xmonad` IRC channel]: https://web.libera.chat/#xmonad
[matrix channel]: https://matrix.to/#/#xmonad:matrix.org
