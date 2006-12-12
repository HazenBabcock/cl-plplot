;
; The Common Lisp / CFFI based interface to the PLplot Scientific Plotting 
; Library.
;
; hazen 12/06
;

This is in fact two packages, one is a "low-level" package (cl-plplot-system) 
that contains the interface to the large and hairy plplot API, and the other is
a (hopefully) easy to use front end (cl-plplot). It was written and tested
with CFFI-0.9.0 and SBCL-0.9.9 on OS-X 10.4. I have also verified that it works
on debian (2.6.15) with CFFI-0.9.1 and SBCL-0.9.12. It should all be Common
Lisp & ASDF installable, but let me know if you find otherwise.

cl-plplot:
This is currently 17 files (in src/window).
1) axis-label.lisp handles the axis-label object.
2) axis.lisp handles the axis object.
3) bar-graph.lisp handles the bar-graph object.
4) classes.lisp specifies all the cl-plplot classes.
5) color-table.lisp handles plplot cmap0 color.
6) contour-plot.lisp handles the contour-plot object.
7) extended-color-table.lisp handles plplot cmap1 color.
8) macros.lisp contains the important macros.
9) package.lisp defines the package & the global variables.
10) plot.lisp defines the plot object as well as providing an interface whereby
     the user can define their own custom plot objects.
11) text-item.lisp handles the text-item object.
12) text-label.lisp handles the text-label object.
13) utility-functions.lisp is a collection of low-level functions.
14) window.lisp handles the window object.
15) x-y-plot.lisp handles the x-y-plot object.
16) 3D-plot.lisp handles the 3D-plot object.
17) 3D-window.lisp handles the 3D-window object.
 
Things should be fairly stable now and the interface should not change much
going forward.


The file src/examples/window-examples.lisp contains examples of how to use 
cl-plplot to generate different 2D plots. The file classes.txt tries to
explain the organization of the various cl-plplot classes.


cl-plplot-system: 
This is currently 4 files (src/system).
1) loadlib.lisp defines the package and loads the PLplot library.
2) defcfun.lisp defines the macro that is used to facilitate wrapping all the
	CFFI calls.
3) misc.lisp contains some helper functions and macros.
4) api.lisp contains all the plplot library / CFFI interface calls. Not
	every function in the plplot API is available, though all the ones that
	are associated with drawing/graphing should be. Others can of course
	be added as desired.

The file system-examples.lisp contains some examples of how you might directly 
use the functions in cl-plplot-system. Other useful resources are the 
documentation that comes with plplot, the examples that come with plplot, the
(admittedly somewhat sparse) comments in api.lisp & possibly plplot.h. You 
should be aware the cl-plplot-system exports lots and lots of symbols 
(approximately 2 for every plplot function). These will generally be of the 
form "c-pl*" or "pl*", so if you like function names that begin with these 
characters then watch out! 

A callback has been added that should trap PLplot when it tries to call exit()
and instead cause a Lisp side error to be thrown. If you find a PLplot error
that is not trapped by this callback please let me know.

