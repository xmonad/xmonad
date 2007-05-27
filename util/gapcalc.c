/*  gapcalc - calculate height of given font
 *  Copyright (C) 2007 by Robert Manea  <rob.manea@gmail.com>
 *
 *  Compile with: cc -lX11 -o gapcalc gapcalc.c
 */

#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

void 
eprint(const char *errstr, ...) {
    va_list ap;

    va_start(ap, errstr);
    vfprintf(stderr, errstr, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}


int 
main(int argc, char *argv[]) {
    Display *dpy;    
    XFontStruct *xfont;
    XFontSet set;
    char *def, **missing;
    char *fontstr;
    int i, n, ascent, descent;

    if(argc < 2)
        eprint("Usage: gapcalc <font>\n");

    if(!(dpy = XOpenDisplay(0)))
        eprint("fatal: cannot open display\n");

    fontstr = argv[1];
    missing = NULL;

    set = XCreateFontSet(dpy, fontstr, &missing, &n, &def);
    if(missing)
        XFreeStringList(missing);
    if(set) {
        XFontSetExtents *font_extents;
        XFontStruct **xfonts;
        char **font_names;
        ascent = descent = 0;
        font_extents = XExtentsOfFontSet(set);
        n = XFontsOfFontSet(set, &xfonts, &font_names);
        for(i = 0, ascent = 0, descent = 0; i < n; i++) {
            if(ascent < (*xfonts)->ascent)
                ascent = (*xfonts)->ascent;
            if(descent < (*xfonts)->descent)
                descent = (*xfonts)->descent;
            xfonts++;
        }
    } else if(!set && (xfont = XLoadQueryFont(dpy, fontstr))) {
        ascent = xfont->ascent;
        descent = xfont->descent;
    } else 
        eprint("fatal: cannot find specified font\n");

    printf("%d\n", ascent + descent + 2);

  
  return EXIT_SUCCESS;
}

