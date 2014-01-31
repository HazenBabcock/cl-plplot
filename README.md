#cl-plplot#
The Common Lisp / CFFI based interface to the [PLplot](http://common-lisp.net/project/cl-plplot/) Scientific Plotting Library.

#Notes#

##08-08-10##
Going forward from release 0.6.0 I'm simply going to be trying to keep theLisp binding in sync with the PLplot library (cl-plplot-system). Due to time constraints, and also because I do not actually use it myself enough to have good ideas about what does and does not work, I'm not planning on any further updates to my attempt to make a more Lispish interface to PLplot (cl-plplot).

#Layout#

##cl-plplot##
This is currently 18 files (in src/window).
1. axis-label.lisp handles the axis-label and 3D-axis-label object.
2. axis.lisp handles the axis object.
3. bar-graph.lisp handles the bar-graph object.
4. classes.lisp specifies all the cl-plplot classes.
5. color-table.lisp handles plplot cmap0 color.
6. contour-plot.lisp handles the contour-plot object.
7. extended-color-table.lisp handles plplot cmap1 color.
8. macros.lisp contains the important macros.
9. package.lisp defines the package & the global variables.
10. plot.lisp defines the plot object as well as providing an interface whereby the user can define their own custom plot objects.
11. surface-plot.lisp handles the surface-plot object.
12. text-item.lisp handles the text-item object.
13. text-label.lisp handles the text-label and 3D-text-label object.
14. utility-functions.lisp is a collection of low-level functions.
15. window.lisp handles the window object.
16. x-y-plot.lisp handles the x-y-plot object.
17. 3D-mesh.lisp handles the 3D-mesh object.
18. 3D-window.lisp handles the 3D-window object.
 
The file src/examples/window-examples.lisp contains examples of how to use 
cl-plplot to generate different 2D plots.


##cl-plplot-system##
This is currently 4 files (src/system).
1. loadlib.lisp defines the package and loads the PLplot library.
2. defcfun.lisp defines the macro that is used to facilitate wrapping all the CFFI calls.
3. misc.lisp contains some helper functions and macros.
4. api.lisp contains all the plplot library / CFFI interface calls. Not every function in the plplot API is available, though all the ones that are associated with drawing/graphing should be. Others can of course be added as desired.

The file system-examples.lisp contains some examples of how you might directly use the functions in cl-plplot-system. Also, src/examples/ contains Lisp versions of all the standard PLplot examples. Other useful resources are the documentation that comes with plplot, the examples that come with plplot, the (admittedly somewhat sparse) comments in api.lisp & possibly plplot.h. You 
should be aware the cl-plplot-system exports lots and lots of symbols (approximately 2 for every plplot function). These will generally be of the form "c-pl*" or "pl*", so if you like function names that begin with these characters then watch out! 

A callback has been added that should trap PLplot when it tries to call exit() and instead cause a Lisp side error to be thrown. If you find a PLplot error that is not trapped by this callback please let me know.

