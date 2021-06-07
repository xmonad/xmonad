# XMonad Maintainers

## The XMonad Core Team

  * Brandon S Allbery [GitHub][geekosaur], IRC: `geekosaur`

  * Brent Yorgey [GitHub][byorgey], IRC: `byorgey`

  * Daniel Wagner [GitHub][dmwit], [Twitter][twitter:dmwit], IRC: `dmwit`

  * Sibi Prabakaran [GitHub][psibi], [Twitter][twitter:psibi], IRC: `sibi`

  * slotThe [GitHub][slotThe], IRC: `Solid`

  * Tomáš Janoušek [GitHub][liskin], [Twitter][twitter:liskin], IRC: `liskin`

[geekosaur]: https://github.com/geekosaur
[byorgey]: https://github.com/byorgey
[dmwit]: https://github.com/dmwit
[psibi]: https://github.com/psibi
[liskin]: https://github.com/liskin
[slotThe]: https://github.com/slotThe

[twitter:dmwit]: https://twitter.com/dmwit13
[twitter:psibi]: https://twitter.com/psibi
[twitter:liskin]: https://twitter.com/Liskni_si

## Hall of Fame (past maintainers/developers)

  * Adam Vogt [GitHub](https://github.com/aavogt)

  * Peter Simons [GitHub](https://github.com/peti), [Twitter](https://twitter.com/OriginalPeti)

  * Spencer Janssen [GitHub](https://github.com/spencerjanssen)

  * Don Stewart [GitHub](https://github.com/donsbot), [Twitter](https://twitter.com/donsbot)

  * David Roundy [GitHub](https://github.com/droundy)

  * Daniel Schoepe [GitHub](https://github.com/dschoepe)

  * Eric Mertens [GitHub](https://github.com/glguy)

  * Nicolas Pouillard [GitHub](https://github.com/np)

  * Roman Cheplyaka [GitHub](https://github.com/UnkindPartition)

  * Gwern Branwen [GitHub](https://github.com/gwern)

  * Lukas Mai [GitHub](https://github.com/mauke)

  * Braden Shepherdson [GitHub](https://github.com/shepheb)

  * Devin Mullins [GitHub](https://github.com/twifkak)

  * David Lazar [GitHub](https://github.com/davidlazar)

  * Peter J. Jones [GitHub](https://github.com/pjones)

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

## Website and Other Accounts

* The [xmonad twitter] is tended to by [liskin].

* The [xmonad.org] domain is owned by [eyenx] and the website itself is
  deployed via GitHub Pages.  It can be updated by making a pull request
  against the [xmonad-web] repository.

[eyenx]: https://github.com/eyenx
[xmonad-web]: https://github.com/xmonad/xmonad-web/
[xmonad.org]: https://xmonad.org/
[xmonad twitter]: https://twitter.com/xmonad
