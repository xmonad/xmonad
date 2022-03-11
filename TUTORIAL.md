# XMonad Configuration Tutorial

We're going to take you, step-by-step, through the process of
configuring xmonad, setting up [xmobar] as a status bar, configuring
[trayer-srg] as a tray, and making it all play nicely together.

We assume that you have read the [xmonad guided tour] already.  It is a
bit dated at this point but, because xmonad is stable, the guide should
still give you a good overview of the most basic functionality.

Before we begin, here are two screenshots of the kind of desktop we will
be creating together.  In particular, a useful layout we'll conjure into
being—the three columns layout from [XMonad.Layout.ThreeColumns] with
the ability to magnify stack windows via [XMonad.Layout.Magnifier]:

<p>
<img alt="blank desktop" src="https://user-images.githubusercontent.com/50166980/111586872-c0eb0e00-87c1-11eb-9129-c2781bbbf227.png" width="550">
<img alt="blank desktop" src="https://user-images.githubusercontent.com/50166980/111586885-c47e9500-87c1-11eb-8e8e-83d4d33c4c8d.png" width="550">
</p>

So let's get started!

## Preliminaries

First you'll want to install xmonad.  You can either do this with your
system's package manager or via `stack`/`cabal`.  The latter may be
necessary if your distribution does not package `xmonad` and
`xmonad-contrib` (or packages a version that's very old) or you want to
use the git versions instead of a tagged release.  You can find
instructions for how to do this in [INSTALL.md].

One more word of warning: if you are on a distribution that installs
every Haskell package dynamically linked—thus essentially breaking
Haskell—(Arch Linux being a prominent example) then we would highly
recommend installing via `stack` or `cabal` as instructed in
[INSTALL.md].  If you still want to install xmonad via your system's
package manager, you will need to `xmonad --recompile` _every time_ a
Haskell dependency is updated—else xmonad may fail to start when you
want to log in!

We're going to assume xmonad version `0.17.0` and xmonad-contrib version
`0.17.0` here, though most of these steps should work with older
versions as well.  When we get to the relevant parts, will point you to
alternatives that work with at least xmonad version `0.15` and
xmonad-contrib version `0.16`.  This will usually be accompanied by a
big "_IF YOU ARE ON A VERSION `< 0.17.0`_", so don't worry about missing
it!

Throughout the tutorial we will use, for keybindings, a syntax very akin
to the [GNU Emacs conventions] for the same thing—so `C-x` means "hold
down the control key and then press the `x` key".  One exception is that
in our case `M` will not necessarily mean Alt (also called `Meta`), but
"your modifier key"; this is Alt by default, although many people map it
to Super instead (I will show you how to do this below).

This guide should work for any GNU/Linux distribution and even for BSD
folks.  Because Debian-based distributions are still rather popular, we
will give you the `apt` commands when it comes to installing software.
If you use another distribution, just substitute the appropriate
commands for your system.

To install xmonad, as well as some utilities, via `apt` you can just run

``` console
$ apt-get install xmonad libghc-xmonad-contrib-dev libghc-xmonad-dev suckless-tools
```

This installs xmonad itself, everything you need to configure it, and
`suckless-tools`, which provides the application launcher `dmenu`.  This
program is used as the default application launcher on `M-p`.

If you are installing xmonad with `stack` or `cabal`, remember to _not_
install `xmonad` and its libraries here!

For the remainder of this document, we will assume that you are running
a live xmonad session in some capacity.  If you have set up your
`~/.xinitrc` as directed in the xmonad guided tour, you should be good
to go!  If not, just smack an `exec xmonad` at the bottom of that file.

## Installing Xmobar

What we need to do now—provided we want to use a bar at all—is to
install [xmobar].  If you visit [xmobar's `Installation` section] you
will find everything from how to install it with your system's package
manager all the way to how to compile it yourself.

We will show you how we make these programs talk to each other a bit
later on.  For now, let's start to explore how we can customize this
window manager of ours!

## Customizing XMonad

Xmonad's configuration file is written in [Haskell]—but don't worry, we
won't assume that you know the language for the purposes of this
tutorial.  The configuration file can either reside within
`$XDG_CONFIG_HOME/xmonad`, `~/.xmonad`, or `$XMONAD_CONFIG_DIR`; see
`man 1 xmonad` for further details (the likes of `$XDG_CONFIG_HOME` is
called a [shell variable]).  Let's use `$XDG_CONFIG_HOME/xmonad` for the
purposes of this tutorial, which resolves to `~/.config/xmonad` on most
systems—the `~/.config` directory is also the place where things will
default to should `$XDG_CONFIG_HOME` not be set.

First, we need to create `~/.config/xmonad` and, in this directory, a
file called `xmonad.hs`.  We'll start off with importing some of the
utility modules we will use.  At the very top of the file, write

``` haskell
import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
```

All of these imports are _unqualified_, meaning we are importing all of
the functions in the respective modules.  For configuration files this
is what most people want.  If you prefer to import things explicitly
you can do so by adding the necessary function to the `import` statement
in parentheses.  For example

``` haskell
import XMonad.Util.EZConfig (additionalKeysP)
```

For the purposes of this tutorial, we will be importing everything
coming from xmonad directly unqualified.

Next, a basic configuration—which is the same as the default config—is
this:

``` haskell
main :: IO ()
main = xmonad def
```

In case you're interested in what this default configuration actually
looks like, you can find it under [XMonad.Config].  Do note that it is
_not_ advised to copy that file and use it as the basis of your
configuration, as you won't notice when a default changes!

You should be able to save the above file, with the import lines plus
the other two and then press `M-q` to load it up.  Another way to
validate your `xmonad.hs` is to simply run `xmonad --recompile` in a
terminal.  You'll see errors (in an `xmessage` popup, so make sure that
is installed on your system) if it's bad, and nothing if it's good.
It's not the end of the world if you restart xmonad and get errors, as
you will still be on your old, working, configuration and have all the
time in the world to fix your errors before trying again!

Let's add a few additional things.  The default modifier key is Alt,
which is also used in Emacs.  Sometimes Emacs and xmonad want to use the
same key for different actions.  Rather than remap every common key,
many people just change Mod to be the Super key—the one between Ctrl and
Alt on most keyboards.  We can do this by changing the above `main`
function in the following way:

``` haskell
main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask  -- Rebind Mod to the Super key
    }
```

The two dashes signify a comment to the end of the line.  Notice the
curly braces; these stand for a [record update] in Haskell (records are
sometimes called "structs" in C-like languages).  What it means is "take
`def` and change its `modMask` field to the thing **I** want".  Taking a
record that already has some defaults set and modifying only the fields
one cares about is a pattern that is often used within xmonad, so it's
worth pausing for a bit here to really take this new syntax in.

Don't mind the dollar sign too much; it essentially serves to split
apart the `xmonad` function and the `def { .. }` record update visually.
Haskell people really don't like writing parentheses, so they sometimes
use a dollar sign instead.  For us this is particularly nice, because
now we don't have to awkwardly write

``` haskell
main :: IO ()
main = xmonad (def
    { modMask = mod4Mask  -- Rebind Mod to the Super key
    })
```

This will be especially handy when adding more combinators; something we
will talk about later on.  The dollar sign is superfluous in this
example, but that will change soon enough so it's worth introducing it
here as well.

What if we wanted to add other keybindings?  Say you also want to bind
`M-S-z` to lock your screen with the screensaver, `M-S-=` to take a
snapshot of one window, and `M-]` to spawn Firefox.  This can be
achieved with the `additionalKeysP` function from the
[XMonad.Util.EZConfig] module—luckily we already have it imported!  Our
config file, starting with `main`, now looks like:

``` haskell
main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask  -- Rebind Mod to the Super key
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    ]
```

That syntax look familiar?

You can find the names for special keys, like `Print` or the `F`-keys,
in the [XMonad.Util.EZConfig] documentation.

We will cover setting up the screensaver later in this tutorial.

The `unGrab` before running the `scrot -s` command tells xmonad to
release its keyboard grab before `scrot -s` tries to grab the keyboard
itself.  The little `*>` operator essentially just sequences two
functions, i.e. `f *> g` says

> first do `f` and then, discarding any result that `f` may have given
> me, do `g`.

Do note that you may need to install `scrot` if you don't have it on
your system already.

What if we wanted to augment our xmonad experience just a little more?
We already have `xmonad-contrib`, which means endless possibilities!
Say we want to add a three column layout to our layouts and also magnify
focused stack windows, making it more useful on smaller screens.

We start by visiting the documentation for [XMonad.Layout.ThreeColumns].
The very first sentence of the `Usage` section tells us exactly what we
want to start with:

> You can use this module with the following in your `~/.xmonad/xmonad.hs`:
>
> `import XMonad.Layout.ThreeColumns`

Ignoring the part about where exactly our `xmonad.hs` is (we have opted
to put it into `~/.config/xmonad/xmonad.hs`, remember?) we can follow
that documentation word for word.  Let's add

``` haskell
import XMonad.Layout.ThreeColumns
```

to the top of our configuration file.  Most modules have a lot of
accompanying text and usage examples in them—so while the type
signatures may seem scary, don't be afraid to look up the
[xmonad-contrib documentation] on Hackage!

Next we just need to tell xmonad that we want to use that particular
layout.  To do this, there is the `layoutHook`.  Let's use the default
layout as a base:

``` haskell
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
```

The so-called `where`-clause above simply consists of local declarations
that might clutter things up where they all declared at the top-level
like this

``` haskell
myLayout = Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2)) ||| Full
```

It also gives us the chance of documenting what the individual numbers
mean!

Now we can add the layout according to the [XMonad.Layout.ThreeColumns]
documentation.  At this point, we would encourage you to try this
yourself with just the docs guiding you.  If you can't do it, don't
worry; it'll come with time!

We can, for example, add the additional layout like this:

``` haskell
myLayout = tiled ||| Mirror tiled ||| Full ||| ThreeColMid 1 (3/100) (1/2)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
```

or even, because the numbers happen to line up, like this:

``` haskell
myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
```

Now we just need to tell xmonad that we want to use this modified
`layoutHook` instead of the default.  Again, try to reason this out for
yourself by just looking at the documentation.  Ready?  Here we go:

``` haskell
main :: IO ()
main = xmonad $ def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , layoutHook = myLayout  -- Use custom layouts
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    ]
```

But we also wanted to add magnification, right?  Luckily for us, there's
a module for that as well!  It's called [XMonad.Layout.Magnifier].
Again, take a look at the documentation before reading on—see if you can
reason out what to do by yourself.  Let's pick the `magnifiercz'`
modifier from the library; it magnifies a window by a given amount, but
only if it's a stack window.  Add it to your three column layout thusly:

``` haskell
myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
```

Don't forget to import the module!

You can think of the `$` here as putting everything into parentheses
from the dollar to the end of the line.  If you don't like that you can
also write

``` haskell
threeCol = magnifiercz' 1.3 (ThreeColMid nmaster delta ratio)
```

instead.

That's it!  Now we have a perfectly functioning three column layout with
a magnified stack.  If you compare this with the starting screenshots,
you will see that that's exactly the behaviour we wanted to achieve!

A last thing that's worth knowing about are so-called "combinators"—at
least we call them that, we can't tell you what to do.  These are
functions that compose with the `xmonad` function and add a lot of hooks
and other things for you (trying to achieve a specific goal), so you
don't have to do all the manual work yourself.  For example, xmonad—by
default—is only [ICCCM] compliant.  Nowadays, however, a lot of programs
(including many compositors) expect the window manager to _also_ be
[EWMH] compliant.  So let's save ourselves a lot of future trouble and
add that to xmonad straight away!

This functionality is to be found in the [XMonad.Hooks.EwmhDesktops]
module, so let's import it:

``` haskell
import XMonad.Hooks.EwmhDesktops
```

We might also consider using the `ewmhFullscreen` combinator.  By
default, a "fullscreened" application is still bound by its window
dimensions; this means that if the window occupies half of the screen
before it was fullscreened, it will also do so afterwards.  Some people
really like this behaviour, as applications thinking they're in
fullscreen mode tend to remove a lot of clutter (looking at you,
Firefox).  However, because a lot of people explicitly do not want this
effect (and some applications, like chromium, will misbehave and need
some [Hacks] to make this work), we will also add the relevant function
to get "proper" fullscreen behaviour here.

_IF YOU ARE ON A VERSION `< 0.17.0`_: The `ewmhFullscreen` function does
  not exist in these versions.  Instead of it, you can try to add
  `fullscreenEventHook` to your `handleEventHook` to achieve similar
  functionality (how to do this is explained in the documentation of
  [XMonad.Hooks.EwmhDesktops]).

To use the two combinators, we compose them with the `xmonad` function
in the following way:

``` haskell
main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , layoutHook = myLayout  -- Use custom layouts
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    ]
```

Do mind the order of the two combinators—by a particularly awkward set
of circumstances, they do not commute!

This `main` function is getting pretty crowded now, so let's refactor it
a little bit.  A good way to do this is to split the config part into
one function and the "main and all the combinators" part into another.
Let's call the config part `myConfig` for... obvious reasons.  It would
look like this:

``` haskell
main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ myConfig

myConfig = def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , layoutHook = myLayout  -- Use custom layouts
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    ]
```

Much better!

## Make XMonad and Xmobar Talk to Each Other

Onto the main dish.  First, we have to import the necessary modules.
Add the following to your list of imports:

``` haskell
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
```

_IF YOU ARE ON A VERSION `< 0.17.0`_: The `XMonad.Hooks.StatusBar` and
  `XMonad.Hooks.StatusBar.PP` modules don't exist yet.  You can find
  everything you need in the `XMonad.Hooks.DynamicLog` module, so remove
  these two imports.

Replace your `main` function above with:

``` haskell
main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ xmobarProp $ myConfig
```

_IF YOU ARE ON A VERSION `< 0.17.0`_: The `xmobarProp` function does not
  exist in these versions.  Instead of it, use `xmobar` via
  `main = xmonad . ewmh =<< xmobar myConfig` and carefully read the part
  about pipes later on (`xmobar` uses pipes to make xmobar talk to
  xmonad).

As a quick side-note, we could have also written

``` haskell
main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig
```

Notice how `$` became `.`!  The dot operator `(.)` in Haskell means
function composition and is read from right to left.  What this means in
this specific case is essentially the following:

> take the four functions `xmonad`, `ewmhFullscreen`, `ewmh`, and
> `xmobarProp` and give me the big new function
> `xmonad . ewmhFullscreen . ewmh . xmobarProp` that first executes
> `xmobarProp`, then `ewmh`, then `ewmhFullscreen`, and finally
> `xmonad`.  Then give it `myConfig` as its argument so it can do its
> thing.

This should strike you as nothing more than a syntactical quirk, at
least in this case.  And indeed, since `($)` is just function
application there is a very nice relationship between `(.)` and `($)`.
This may be more obvious if we write everything with parentheses and
apply the `(.)` operator (because we do have an argument):

``` haskell
-- ($) version
main = xmonad $ ewmhFullscreen $ ewmh $ xmobarProp $ myConfig
-- ($) version with parentheses
main = xmonad (ewmhFullscreen (ewmh (xmobarProp (myConfig))))

-- (.) version with parentheses
main = (xmonad . ewmhFullscreen . ewmh . xmobarProp) (myConfig)
-- xmobarProp applied
main = (xmonad . ewmhFullscreen . ewmh) (xmobarProp (myConfig))
-- ewmh applied
main = (xmonad . ewmhFullscreen) (ewmh (xmobarProp (myConfig)))
-- xmonad and ewmhFullscreen applied
main = (xmonad (ewmhFullscreen (ewmh (xmobarProp (myConfig))))
```

It's the same!  This is special to the interplay with `(.)` and `($)`
though; if you're on an older version of xmonad and xmonad-contrib and
use `xmobar` instead of `xmobarProp`, then you _have_ to write

``` haskell
main = xmonad . ewmhFullscreen . ewmh =<< xmobar myConfig
```

and this is _not_ equivalent to

``` haskell
main = xmonad (ewmhFullscreen (ewmh =<< xmobar myConfig))
```

Consult a Haskell book of your choice for why this is the case.

Back to our actual goal: customizing xmonad.  What the code we've
written does is take our tweaked default configuration `myConfig` and
add the support we need to make xmobar our status bar.  Do note that you
will also need to add the `XMonadLog` plugin to your xmobar
configuration; we will do this together below, so don't sweat it for
now.

To understand why this is necessary, let's talk a little bit about how
xmonad and xmobar fit together.  You can make them talk to each other in
several different ways.

By default, xmobar accepts input on its stdin, which it can display at
an arbitrary position on the screen.  We want xmonad to send xmobar the
stuff that you can see at the upper left of the starting screenshots:
information about available workspaces, current layout, and open
windows.  Naïvely, we can achieve this by spawning a pipe and letting
xmonad feed the relevant information to that pipe.  The problem with
that approach is that when the pipe is not being read and gets full,
xmonad will freeze!

It is thus much better to switch over to property based logging, where
we are writing to an X11 property and having xmobar read that; no danger
when things are not being read!  For this reason we have to use
`XMonadLog` instead of `StdinReader` in our xmobar.  There's also an
`UnsafeXMonadLog` available, should you want to send actions to xmobar
(this is useful, for example, for [XMonad.Util.ClickableWorkspaces],
which is a new feature in `0.17.0`).

_IF YOU ARE ON A VERSION `< 0.17.0`_: As discussed above, the `xmobar`
  function uses pipes, so you actually do want to use the `StdinReader`.
  Simply replace _all_ occurences of `XMonadLog` with `StdinReader`
  below (don't forget the template!)

## Configuring Xmobar

Now, before this will work, we have to configure xmobar.  Here's a nice
starting point.  Be aware that, while Haskell syntax highlighting is
used here to make this pretty, xmobar's config is _not_ a Haskell file
and thus can't execute arbitrary code—at least not by default.  If you
do want to configure xmobar in Haskell there is a note about that at the
end of this section.

``` haskell
Config { overrideRedirect = False
       , font     = "xft:iosevka-9"
       , bgColor  = "#5f5f5f"
       , fgColor  = "#f8f8f2"
       , position = TopW L 90
       , commands = [ Run Weather "EGPF"
                        [ "--template", "<weather> <tempC>°C"
                        , "-L", "0"
                        , "-H", "25"
                        , "--low"   , "lightblue"
                        , "--normal", "#f8f8f2"
                        , "--high"  , "red"
                        ] 36000
                    , Run Cpu
                        [ "-L", "3"
                        , "-H", "50"
                        , "--high"  , "red"
                        , "--normal", "green"
                        ] 10
                    , Run Alsa "default" "Master"
                        [ "--template", "<volumestatus>"
                        , "--suffix"  , "True"
                        , "--"
                        , "--on", ""
                        ]
                    , Run Memory ["--template", "Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                    , Run XMonadLog
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%XMonadLog% }{ %alsa:default:Master% | %cpu% | %memory% * %swap% | %EGPF% | %date% "
       }
```

First, we set the font to use for the bar, as well as the colors.  The
position options are documented well on the [xmobar home page] or,
alternatively, in the [quick-start.org] on GitHub.  The particular
option of `TopW L 90` says to put the bar in the upper left of the
screen, and make it consume 90% of the width of the screen (we need to
leave a little bit of space for `trayer-srg`).  If you're up for it—and
this really requires more shell-scripting than Haskell knowledge—you can
also try to seamlessly embed trayer into xmobar by using
[trayer-padding-icon.sh] and following the advice given in that thread.

In the commands list you, well, define commands.  Commands are the
pieces that generate the content to be displayed in your bar.  These
will later be combined together in the `template`.  Here, we have
defined a weather widget, a CPU widget, memory and swap widgets, a date,
a volume indicator, and of course the data from xmonad via `XMonadLog`.

The `EGPF` in the weather command is a particular station.  Replace both
(!) occurences of it with your choice of ICAO weather stations.  For a
list of ICAO codes you can visit the relevant [Wikipedia page].  You can
of course monitor more than one if you like; see xmobar's [weather
monitor] documentation for further details.

The `template` then combines everything together.  The `alignSep`
variable controls the alignment of all of the monitors.  Stuff to be
left-justified goes before the `}` character, things to be centered
after it, and things to be right justified after `{`.  We have nothing
centered so there is nothing in-between them.

Save the file to `~/.xmobarrc`.  Now press `M-q` to reload xmonad; you
should now see xmobar with your new configuration!  Please note that, at
this point, the config _has_ to reside in `~/.xmobarrc`.  We will,
however, discuss how to change this soon.

It is also possible to completely configure xmobar in Haskell, just like
xmonad.  If you want to know more about that, you can check out the
[xmobar.hs] example in the official documentation.  For a more
complicated example, you can also check out [jao's xmobar.hs] (he's the
current maintainer of xmobar).

## Changing What XMonad Sends to Xmobar

Now that the xmobar side of the picture looks nice, what about the stuff
that xmonad sends to xmobar?  It would be nice to visually match these
two.  Sadly, this is not quite possible with our `xmobarProp` function;
however, looking at the implementation of the function (or, indeed, the
top-level documentation of the module!) should give us some ideas for
how to proceed:

``` haskell
xmobarProp config =
  withEasySB (statusBarProp "xmobar" (pure xmobarPP)) toggleStrutsKey config
```

This means that `xmobarProp` just calls the functions `withEasySB` and
`statusBarProp` with some arguments; crucially for us, notice the
`xmobarPP`.  In this context "PP" stands for "pretty-printer"—exactly
what we want to modify!

Let's copy the implementation over into our main function:

``` haskell
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure def)) defToggleStrutsKey
     $ myConfig
```

_IF YOU ARE ON A VERSION `< 0.17.0`_: `xmobar` has a similar definition,
  relying on `statusBar` alone: `xmobar = statusBar "xmobar" xmobarPP
    toggleStrutsKey`.  Sadly, the `defToggleStrutsKey` function is not yet
  exported, so you will have to define it yourself:

``` haskell
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
   =<< statusBar "xmobar" def toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
```

The `defToggleStrutsKey` here is just the key with which you can toggle
the bar; it is bound to `M-b`.  If you want to change this, you can also
define your own:

``` haskell
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure def)) toggleStrutsKey
     $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
```

Feel free to change the binding by modifying the `(m, xK_b)` tuple to
your liking.

If you want your xmobar configuration file to reside somewhere else than
`~/.xmobarrc`, you can now simply give the file to xmobar as a
positional argument!  For example:

``` haskell
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure def)) defToggleStrutsKey
     $ myConfig
```

Back to controlling what exactly we send to xmobar.  The `def`
pretty-printer just gives us the same result that the internal
`xmobarPP` would have given us.  Let's try to build something on top of
this.  To prepare, we can first create a new function `myXmobarPP` with
the default configuration:

``` haskell
myXmobarPP :: PP
myXmobarPP = def
```

and then plug that into our main function:

``` haskell
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig
```

As before, we now change things by modifying that `def` record until we
find something that we like.  First, for some functionality that we need
further down we need to import one more module:

``` haskell
import XMonad.Util.Loggers
```

Now we are finally ready to make things pretty.  There are _a lot_ of
options for the [PP record]; I'd advise you to read through all of them
now, so you don't get lost!

``` haskell
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
```

_IF YOU ARE ON A VERSION `< 0.17`_: Both `logTitles` and `xmobarBorder`
  are not available yet, so you will have to remove them.  As an
  alternative to `xmobarBorder`, a common way to "mark" the currently
  focused workspace is by using brackets; you can try something like
  `ppCurrent = wrap (blue "[") (blue "]")` and see if you like it.  Also
  read the bit about `ppOrder` further down!

That's a lot!  But don't worry, take a deep breath and remind yourself
of what you read above in the documentation of the [PP record].  Even if
you haven't read the documentation yet, most of the fields should be
pretty self-explanatory; `ppTitle` formats the title of the currently
focused window, `ppCurrent` formats the currently focused workspace,
`ppHidden` is for the hidden workspaces that have windows on them, etc.
The rest is just deciding on some pretty colours and formatting things
just how we like it.

An important thing to talk about may be `ppOrder`.  Quoting from its
documentation:

> By default, this function receives a list with three formatted
> strings, representing the workspaces, the layout, and the current
> window title, respectively. If you have specified any extra loggers
> in ppExtras, their output will also be appended to the list.

So the first three argument of the list (`ws`, `l`, and `_` in our case,
where the `_` just means we want to ignore that argument and not give it
a name) are the workspaces to show, the currently active layout, and the
title of the focused window.  The last element—`wins`—is what we gave to
`ppExtras`; if you added more loggers to that then you would have to add
more items to the list, like this:

``` haskell
ppOrder = \[ws, l, _, wins, more, more2] -> [ws, l, wins, more, more2]
```

However, many people want to show _all_ window titles on the currently
focused workspace instead.  For that, one can use `logTitles` from
[XMonad.Util.Loggers] (remember that module we just imported?).
However, `logTitles` logs _all_ titles.  Naturally, we don't want to
show the focused window twice and so we suppress it here by ignoring the
third argument of `ppOrder` and not returning it.  The functions
`formatFocused` and `formatUnfocused` should be relatively self
explanitory—they decide how to format the focused resp. unfocused
windows.

By the way, the `\ ... ->` syntax in there is Haskell's way to express a
[lambda abstraction] (or anonymous function, as some languages call it).
All of the arguments of the function come directly after the `\` and
before the `->`; in our case, this is a list with exactly four elements
in it.  Basically, it's a nice way to write a function inline and not
having to define it inside e.g. a `where` clause.  The above could have
also be written as

``` haskell
myXmobarPP :: PP
myXmobarPP = def
    { -- stuff here
    , ppOrder = myOrder
      -- more stuff here
    }
  where
    myOrder [ws, l, _, wins] = [ws, l, wins]
    -- more stuff here
```

If you're unsure of the number of elements that your `ppOrder` will
take, you can also specify the list like this:

``` haskell
ppOrder = \(ws : l : _ : wins : _) -> [ws, l, wins]
```

This says that it is a list of _at least_ four elements (`ws`, `l`, the
unnamed argument, and `wins`), but that afterwards everything is
possible.

This config is really quite complicated.  If this is too much for you,
you can also really just start with the blank

``` haskell
myXmobarPP :: PP
myXmobarPP = def
```

then add something, reload xmonad, see how things change and whether you
like them.  If not, remove that part and try something else.  If you do,
try to understand how that particular piece of code works.  You'll have
something approaching the above that you fully understand in no time!

## Configuring Related Utilities

So now you've got a status bar and xmonad.  We still need a few more
things: a screensaver, a tray for our apps that have tray icons, a way
to set our desktop background, and the like.

For this, we will need a few pieces of software.

``` shell
apt-get install trayer xscreensaver
```

If you want a network applet, something to set your desktop background,
and a power-manager:

``` shell
apt-get install nm-applet feh xfce4-power-manager
```

First, configure xscreensaver how you like it with the
`xscreensaver-demo` command.  Now, we will set these things up in
`~/.xinitrc` (we could also do most of this in xmonad's `startupHook`,
but `~/.xinitrc` is perhaps more standard).  If you want to use xmonad
with a desktop environment, see [Basic Desktop Environment Integration]
for how to do this.

Your `~/.xinitrc` may wind up looking like this:

``` shell
#!/bin/sh

# [... default stuff that your distro may throw in here ...] #

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x5f5f5f --height 18 &

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr

# Set a nice background
feh --bg-fill --no-fehbg ~/.wallpapers/haskell-red-noise.png

# Fire up screensaver
xscreensaver -no-splash &

# Power Management
xfce4-power-manager &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

exec xmonad
```

Notice the call to `trayer` above.  The options tell it to go on the top
right, with a default width of 10% of the screen (to nicely match up
with xmobar, which we set to a width of 90% of the screen).  We give it
a color and a height.

Then we fire up the rest of the programs that interest us.

Finally, we start xmonad.

<img alt="blank desktop" src="https://user-images.githubusercontent.com/50166980/111529498-84d49080-8762-11eb-9e81-c15dd844b0a9.png" width="660">

Mission accomplished!

Of course substitute the wallpaper for one of your own.  If you like the
one used above, you can find it [here](https://i.imgur.com/9MQHuZx.png).

## Final Touches

There may be some programs that you don't want xmonad to tile.  The
classic example here is the [GNU Image Manipulation Program]; it pops up
all sorts of new windows all the time, and they work best at defined
sizes.  It makes sense for xmonad to float these kinds of windows by
default.

This kind of behaviour can be achieved via the `manageHook`, which runs
when windows are created.  There are several functions to help you match
on a certain window in [XMonad.ManageHook].  For example, suppose we'd
want to match on the class name of the application.  With the
application open, open another terminal and invoke the `xprop` command.
Then click on the application that you would like to know the properties
of.  In our case you should see (among other things)

``` shell
WM_CLASS(STRING) = "gimp", "Gimp"
```

The second string in `WM_CLASS` is the class name, which we can access
with `className` from [XMonad.ManageHook].  The first one is usually
called the instance name and is matched-on via `appName` from the same
module.

Let's use the class name for now.  We can tell all windows with that
class name to float by defining the following manageHook:

``` haskell
myManageHook = (className =? "Gimp" --> doFloat)
```

Say we also want to float all dialog windows.  This is easy with the
`isDialog` function from [XMonad.Hooks.ManageHelpers] (which you should
import) and a little modification to the `myManageHook` function:

``` haskell
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]
```

Now we just need to tell xmonad to actually use our manageHook.  This is
as easy as overriding the `manageHook` field in `myConfig`.  You can do
it like this:

``` haskell
myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    ]
```

## The Whole Thing

The full `~/.config/xmonad/xmonad.hs`, in all its glory, now looks like
this:

``` haskell
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.EwmhDesktops


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
```

## Further Customizations

The following section mostly consists of extra bits—feel free to skip
this and jump directly to [Get in Touch](#get-in-touch).  We've covered
a lot of ground here and sometimes it's really better to let things
settle a bit before going further; much more so if you're happy with how
things are looking and feeling right now!

### Beautifying Xmobar

A usual starting point for beautifying xmobar is either to use `xpm`
icons, or a font like `font-awesome` to add icons to the rest of the
text.  We will show you how to do the latter.  Xmobar, thankfully, makes
this very easy; just put a font down under `additionalFonts` and wrap
your Icons with `<fn>` tags and the respective index of the font
(starting from `1`).  As an example, consider how we would extend our
configuration above with this new functionality:

``` haskell
Config { overrideRedirect = False
       , font             = "xft:iosevka-9"
       , additionalFonts  = ["xft:FontAwesome-9"]
       ...
       , Run Battery
           [ ...
           , "--lows"   , "<fn=1>\62020</fn>  "
           , "--mediums", "<fn=1>\62018</fn>  "
           , "--highs"  , "<fn=1>\62016</fn>  "
             ...
           ]
       ...
       }
```

For an explanation of the battery commands used above, see xmobar's
[battery] documentation.

You can also specify workspaces in the same way and feed them to xmobar
via the property (e.g. have `"<fn=1>\xf120</fn>"` as one of your
workspace names).

As an example how this would look like in a real configuration, you can
look at [Liskin's], [slotThe's], or [TheMC47's] xmobar configuration.
Do note that the last two are Haskell-based and thus may be a little
hard to understand for newcomers.

[Liskin's]: https://github.com/liskin/dotfiles/blob/home/.xmobarrc
[TheMC47's]: https://github.com/TheMC47/dotfiles/tree/master/xmobar/xmobarrc
[slotThe's]: https://gitlab.com/slotThe/dotfiles/-/blob/master/xmobar/.config/xmobarrc/src/xmobarrc.hs

### Renaming Layouts

`Magnifier NoMaster ThreeCol` is quite a mouthful to show in your bar,
right?  Thankfully there is the nifty [XMonad.Layout.Renamed], which
makes renaming layouts easy!  We will focus on the `Replace` constructor
here, as a lot of people will find that that's all they need.  To use it
we again follow the documentation (try it yourself!)—import the module
and then change `myLayout` like this:

``` haskell
myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol
        = renamed [Replace "ThreeCol"]
        $ magnifiercz' 1.3
        $ ThreeColMid nmaster delta ratio
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
```

The new line `renamed [Replace "ThreeCol"]` tells the layout to throw
its current name away and use `ThreeCol` instead.  After reloading
xmonad, you should now see this shorter name in your bar.  The line
breaks here are just cosmetic, by the way; if you want you can write
everything in one line:

``` haskell
threeCol = renamed [Replace "ThreeCol"] $ magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
```

## Get in Touch

The `irc.libera.chat/#xmonad` channel is very friendly and helpful.  It
is possible that people will not immediately answer—we do have lives as
well, after all :).  Eventually though, people will usually chime in if
they have something helpful to say; sometimes this takes 10 minutes,
other times it may well take 10 hours.  If you don't have an IRC client
ready to go, the easiest way to join is via [webchat]—just jot down a
username and you should be good to go!  There is a [log] of the channel
available, in case you do have to disconnect at some point, so don't
worry about missing any messages.

If you're not a fan of real-time interactions, you can also post to the
[xmonad mailing list] or the [xmonad subreddit].

## Trouble?

Check `~/.xsession-errors` or your distribution's equivalent first.  If
you're using a distribution that does not log into a file automatically,
you will have to set this up manually.  For example, you could put
something like

``` shell
if [[ ! $DISPLAY ]]; then
  exec startx >& ~/.xsession-errors
fi
```

into your `~/.profile` file to explicitly log everything into
`~/.xsession-errors`.

If you can't figure out what's wrong, don't hesitate to
[get in touch](#get-in-touch)!

## Closing Thoughts

That was quite a ride!  Don't worry if you didn't understand everything
perfectly, these things take time.  You can re-read parts of this guide
as often as you need to and—with the risk of sounding like a broken
record—if you can't figure something out really do not be afraid to
[get in touch](#get-in-touch).

If you want to see a few more complicated examples of other peoples
xmonad configurations, look no further!  Below are (in alphabetical
order) the configurations of a few of xmonad's maintainers.  Just keep
in mind that these setups are very customized and perhaps a little bit
hard to replicate (some may rely on features only available in personal
forks or git), may or may not be documented, and most aren't very pretty
either :)

  - [byorgey](https://github.com/byorgey/dotfiles)
  - [geekosaur](https://github.com/geekosaur/xmonad.hs/tree/pyanfar)
  - [liskin](https://github.com/liskin/dotfiles/tree/home/.xmonad)
  - [psibi](https://github.com/psibi/dotfiles/tree/master/xmonad)
  - [slotThe](https://gitlab.com/slotThe/dotfiles/-/tree/master/xmonad/.config/xmonad)
  - [TheMC47](https://github.com/TheMC47/dotfiles/tree/master/xmonad/.xmonad)


[log]: https://ircbrowse.tomsmeding.com/browse/lcxmonad
[EWMH]: https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html
[ICCCM]: https://tronche.com/gui/x/icccm/
[webchat]: https://web.libera.chat/#xmonad
[about xmonad]: https://xmonad.org/about.html
[shell variable]: https://www.shellscript.sh/variables1.html
[xmonad-testing]: https://github.com/xmonad/xmonad-testing
[xmonad subreddit]: https://old.reddit.com/r/xmonad/
[xmonad guided tour]: https://xmonad.org/tour.html
[xmonad mailing list]: https://mail.haskell.org/mailman/listinfo/xmonad
[xmonad's GitHub page]: https://github.com/xmonad/xmonad
[trayer-padding-icon.sh]: https://github.com/jaor/xmobar/issues/239#issuecomment-233206552
[xmonad-contrib documentation]: https://hackage.haskell.org/package/xmonad-contrib
[GNU Image Manipulation Program]: https://www.gimp.org/
[Basic Desktop Environment Integration]: https://wiki.haskell.org/Xmonad/Basic_Desktop_Environment_Integration

[Hacks]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-Hacks.html
[PP record]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-DynamicLog.html#t:PP
[INSTALL.md]: INSTALL.md
[XMonad.Config]: https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
[XMonad.ManageHook]: https://xmonad.github.io/xmonad-docs/xmonad/XMonad-ManageHook.html
[XMonad.Util.Loggers]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-Loggers.html
[XMonad.Util.EZConfig]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
[XMonad.Layout.Renamed]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Layout-Renamed.html
[XMonad.Layout.Magnifier]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Layout-Magnifier.html
[XMonad.Doc.Contributing]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Doc-Configuring.html
[XMonad.Hooks.EwmhDesktops]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-EwmhDesktops.html
[XMonad.Layout.ThreeColumns]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Layout-ThreeColumns.html
[XMonad.Hooks.ManageHelpers]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-ManageHelpers.html
[XMonad.Util.ClickableWorkspaces]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-ClickableWorkspaces.html

[xmobar]: https://xmobar.org/
[battery]: https://github.com/jaor/xmobar/blob/master/doc/plugins.org#batteryp-dirs-args-refreshrate
[xmobar.hs]: https://github.com/jaor/xmobar/blob/master/examples/xmobar.hs
[Wikipedia page]: https://en.wikipedia.org/wiki/ICAO_airport_code#Prefixes
[quick-start.org]: https://github.com/jaor/xmobar/blob/master/doc/quick-start.org#configuration-options
[jao's xmobar.hs]: https://codeberg.org/jao/xmobar-config
[weather monitor]: https://github.com/jaor/xmobar/blob/master/doc/plugins.org#weather-monitors
[xmobar home page]: https://xmobar.org/
[xmobar's `Installation` section]: https://github.com/jaor/xmobar#installation

[Haskell]: https://www.haskell.org/
[trayer-srg]: https://github.com/sargon/trayer-srg
[record update]: http://learnyouahaskell.com/making-our-own-types-and-typeclasses
[lambda abstraction]: https://wiki.haskell.org/Lambda_abstraction
[GNU Emacs conventions]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequences.html#Key-Sequences
