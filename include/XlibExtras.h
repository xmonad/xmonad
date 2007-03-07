/* This file copied from the X11 package */

/* -----------------------------------------------------------------------------
 * Definitions for package `X11' which are visible in Haskell land.
 * ---------------------------------------------------------------------------*
 */

#ifndef XLIBEXTRAS_H
#define XLIBEXTRAS_H
#include <stdlib.h>
/* This doesn't always work, so we play safe below... */
#define XUTIL_DEFINE_FUNCTIONS
#include <X11/X.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
/* Xutil.h overrides some functions with macros.
 * In recent versions of X this can be turned off with
 *      #define XUTIL_DEFINE_FUNCTIONS
 * before the #include, but this doesn't work with older versions.
 * As a workaround, we undef the macros here.  Note that this is only
 * safe for functions with return type int.
 */
#undef XDestroyImage
#undef XGetPixel
#undef XPutPixel
#undef XSubImage
#undef XAddPixel
#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>
#endif
