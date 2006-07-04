;
; The Common Lisp / CFFI based interface to the PLplot Scientific Plotting 
; Library.
;
; hazen 4/06
;

This is in fact two packages, one is a "low-level" package (cl-plplot-system) 
that contains the interface to the large and hairy plplot API, and the other is
a (hopefully) easy to use front end (cl-plplot). It was written and tested
with CFFI-0.9.0 and SBCL-0.9.9 on OS-X 10.4. I have also verified that it works
on debian (2.6.15) with CFFI-0.9.1 and SBCL-0.9.12. It should all be Common
Lisp & ASDF installable, but I'm a little new to these things so mistakes may 
have been made.

cl-plplot:
This is currently 10 files (src/window).
1) axis-label.lisp handles the axis-label object.
2) axis.lisp handles the axis object.
3) classes.lisp specifies all the cl-plplot classes.
4) macros.lisp contains the important macros.
5) package.lisp defines the package & the global variables.
6) text-item.lisp handles the text-item object.
7) text-label.lisp handles the text-label object.
8) utility-functions.lisp is a collection of low-level functions.
9) window.lisp handles the window object.
10) x-y-plot.lisp handles the x-y-plot object.

This is still a work in progress so some things may be changed, particularly
regarding the interface that has been provided to manipulate objects of 
these various classes.

The file src/examples/window-examples.lisp contains examples of how to use 
cl-plplot to generate different 2D plots. The file classes.txt tries to
explain the organization of the various cl-plplot classes.


cl-plplot-system: 
This is currently 4 files (src/system).
1) loadlib.lisp defines the package and loads the plplot library as well as
	the standard C library.
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

You should use caution when using the plplot library as some of its functions
have the nasty habit of calling exit() when they get the wrong arguments. This
behavior is really hard to trap & will typically result in your Lisp process
also exiting. It is a good idea to test your plotting function first before
running any really long processes whose results you want to plot.


