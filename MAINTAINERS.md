# XMonad Maintainers

## The XMonad Core Team

  * Adam Vogt [GitHub][aavogt]

  * Brandon S Allbery [GitHub][geekosaur], IRC: `geekosaur`

  * Brent Yorgey [GitHub][byorgey], IRC: `byorgey`

  * Daniel Wagner [GitHub][dmwit], IRC: `dmwit`

  * David Lazar [GitHub][davidlazar]

  * Devin Mullins [GitHub][twifkak]

  * Peter J. Jones [GitHub][pjones], [Twitter][twitter:pjones], [OpenPGP Key][pgp:pjones], IRC: `pmade`

## Release Procedures

When the time comes to release another version of XMonad and Contrib...

  1. Create a release branch (e.g., `release-0.XX`).

     This will allow you to separate the release process from main
     development.  Changes you make on this branch will be merged back
     into `master` as one of the last steps.

  2. Update the version number in the `*.cabal` files and verify
     dependencies and documentation.  This includes the `tested-with:`
     field.

  3. Use the [packdeps][] tool to ensure you have the dependency
     versions correct.  If you need to update the version of a
     dependency then you should rebuild and retest.

  4. Review documentation files and make sure they are accurate:

     - `README.md`
     - `CHANGES.md`
     - and the `example-config.hs` in the `xmonad-testing` repo

  5. Generate the manpage:

     * `cabal configure` with the `-fgeneratemanpage` flag
     * Build the project
     * Run the `generatemanpage` tool from the top level of this repo
     * Review the man page: `man -l man/xmonad.1`

  6. Tag the repository with the release version (e.g., `v0.13`)

  7. Build the project tarballs (`cabal sdist`)

  8. Upload the packages to Hackage (`cabal upload`)

  9. Merge the release branches into `master`

  10. Update the website:

      * Generate and push haddocks with `xmonad-web/gen-docs.sh`

      * Check that `tour.html` and `intro.html` are up to date, and
        mention all core bindings

  11. Update the topic for the IRC channel (`#xmonad`)

  12. Send the `announce-0.XX.txt` file to:

      - XMonad mailing list
      - Haskell Cafe

[packdeps]: http://hackage.haskell.org/package/packdeps

[aavogt]: https://github.com/orgs/xmonad/people/aavogt
[geekosaur]: https://github.com/orgs/xmonad/people/geekosaur
[byorgey]: https://github.com/orgs/xmonad/people/byorgey
[dmwit]: https://github.com/orgs/xmonad/people/dmwit
[davidlazar]: https://github.com/orgs/xmonad/people/davidlazar
[twifkak]: https://github.com/orgs/xmonad/people/twifkak

[pjones]: https://github.com/orgs/xmonad/people/pjones
[twitter:pjones]: https://twitter.com/contextualdev
[pgp:pjones]: http://pgp.mit.edu/pks/lookup?op=get&search=0x526722D1204284CB
