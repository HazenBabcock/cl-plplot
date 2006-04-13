;
; The Common Lisp / CFFI based interface to the PLplot Scientific Plotting 
; Library.
;
; hazen 4/06
;

This is in fact two packages, one is a "low-level" package (cl-plplot-system) 
that contains the interface to the large and hairy plplot API, and the other is
a (hopefully) easy to use front end (cl-plplot). It was written and tested
with CFFI-0.9.0 and SBCL-0.9.9. It should all be Common Lisp & ASDF installable
but I'm a little new to these things so mistakes may have been made.


cl-plplot-system: 
This is currently 4 files.
1) pl-loadlib.lisp defines the package and loads the plplot library as well as
	the standard C library.
2) pl-defcfun.lisp defines the macro that is used to facilitate wrapping all the
	CFFI calls.
3) pl-misc.lisp contains some helper functions and macros.
4) pl-api.lisp contains all the plplot library / CFFI interface calls. Not
	every function in the plplot API is available, though all the ones that
	are associated with drawing/graphing should be. Others can of course
	be added as desired.

The file pl-examples.lisp contains some examples of how you might directly use
the functions in cl-plplot-system. Other useful resources are the 
documentation that comes with plplot, the examples that come with plplot, the
(admittedly somewhat sparse) comments in pl-api.lisp & possibly plplot.h. You 
should be aware the cl-plplot-system exports lots and lots of symbols 
(approximately 2 for every plplot function). These will generally be of the 
form "c-pl*" or "pl*", so if you like function names that begin with these 
characters then watch out! 

You should use caution when using the plplot library as some of its functions
have the nasty habit of calling exit() when they get the wrong arguments. This
behavior is really hard to trap & will typically result in your Lisp process
also exiting. It is a good idea to test your plotting function first before
running any really long processes whose results you want to plot.


cl-plot:
This is currently 1 file.
1) 2D-plot.lisp is the high level interface to plplot. In its current form
	it only makes it easy to make 2D plots and bar graphs. The long term 
	goal is to expand this is to cover other types of plots that people 
	might typically make. This will probably done on a as requested / 
	needed by me basis, so let me know what you are interested in.

The file 2D-plot-examples.lisp contains some examples of how to use the
functions and macros in 2D-plot.lisp.

