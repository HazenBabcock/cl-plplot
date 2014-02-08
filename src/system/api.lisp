;;;;
;;;; Calls to the plplot API. An attempt was made to have the pl-defcfun macro
;;;; handle as many as these as possible, but eventually I gave up and just wrote 
;;;; out some of the less conventional ones. As a consequence, not all the 'raw'
;;;; cffi calls will be name "c-plfunctionname", they are likely to instead be
;;;; "c-pl-plfunctionname".
;;;;
;;;; Doc-strings are from the api.xml file in PLplot.
;;;;
;;;; hazen 01/14
;;;;

(in-package #:cl-plplot-system)

;;;
;;; plplot API functions, ordered alphabetically by PLplot function name
;;;

(pl-defcfun ("c_pladv" pladv) :void
    "Advance the (sub-)page."
  (page plint))


(pl-defcfun ("c_plarc" plarc) :void
    "Draw a circular or elliptical arc."
  (x plflt)
  (y plflt)
  (a plflt)
  (b plflt)
  (angle1 plflt)
  (angle2 plflt)
  (fill plbool))


(pl-defcfun ("c_plbop" plbop) :void
    "Begin a new page.")


(pl-defcfun ("c_plbox" plbox) :void
    "Draw a box with axes, etc."
  (xopt plstr)
  (xtick plflt)
  (nxsub plint)
  (yopt plstr)
  (ytick plflt)
  (nysub plint))


(pl-defcfun ("c_plbox3" plbox3) :void 
    "Draw a box with axes, etc, in 3-d."
  (xopt plstr)
  (xlabel plstr)
  (xtick plflt)
  (nsubx plint)
  (yopt plstr)
  (ylabel plstr)
  (ytick plflt)
  (nsuby plint)
  (zopt plstr)
  (zlabel plstr)
  (ztick plflt)
  (nsubz plint))


(pl-defcfun ("c_plcol0" plcol0) :void
    "Set color, map0."
  (icol0 plint))


(pl-defcfun ("c_plend" plend) :void
    "End plotting session.")


(pl-defcfun ("c_plend1" plend1) :void
    "End plotting session for current stream.")


(pl-defcfun ("c_plenv" plenv) :void 
    "Set up standard window and draw box."
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt)
  (just plint)
  (axis plint))


(pl-defcfun ("c_pleop" pleop) :void
    "Eject current page.")


;; These are helper functions for plf... functions, they
;; return a pointer to a "zops" function.
(defcfun "plf2ops_c" :pointer)
(export 'plf2ops-c)

(defcfun "plf2ops_grid_c" :pointer)
(export 'plf2ops-grid-c)

(defcfun "plf2ops_grid_row_major" :pointer)
(export 'plf2ops-grid-row-major)

(defcfun "plf2ops_grid_col_major" :pointer)
(export 'plf2ops-grid-col-major)


(pl-defcfun ("c_plfont" plfont) :void 
    "Set character font."
  (ifont plint))


(pl-defcfun ("c_plfontld" plfontld) :void 
    "Load character font"
  (fnt plint))


;; See x08l.lisp for an example of how to use this function.
(pl-defcfun ("plfsurf3d" plfsurf3d) :void
    "Plot shaded 3-d surface plot"
  (x *plflt)
  (y *plflt)
  (zops :pointer)
  (zp :pointer)
  (nx plint (length x) nil)
  (ny plint (length y) nil)
  (opt plint)
  (clevel *plflt)
  (nlevel plint (pl-length clevel) nil))


(pl-defcfun ("c_plgcol0" plgcol0) :void 
    "Returns 8-bit RGB values for given color from color map0."
  (icol0 plint)
  (r *plint 1)
  (g *plint 1)
  (b *plint 1))


(defcfun ("c_plgver" c-plgver) :void
  (p_ver :pointer))

(defun plgver ()
  "Get the current library version number."
  (with-foreign-string (version-string (make-array 80 :element-type 'character))
    (c-plgver version-string)
    (foreign-string-to-lisp version-string)))

(export 'plgver)


(pl-defcfun ("c_plhist" plhist) :void
    "Plot a histogram from unbinned data."
  (n plint (length data) nil)
  (data *plflt)
  (datmin plflt)
  (datmax plflt)
  (nbin plint)
  (opt plint))


(pl-defcfun ("c_plhlsrgb" plhlsrgb) :void
    "Convert HLS color to RGB."
  (h plflt)
  (l plflt)
  (s plflt)
  (p_r *plflt 1)
  (p_g *plflt 1)
  (p_b *plflt 1))


(pl-defcfun ("c_plinit" plinit) :void
    "Initialize PLplot.")


(pl-defcfun ("c_pljoin" pljoin) :void
    "Draw a line between two points."
  (x1 plflt)
  (y1 plflt)
  (x2 plflt)
  (y2 plflt))


(pl-defcfun ("c_pllab" pllab) :void 
    "Simple routine to write labels."
  (xlabel plstr)
  (ylabel plstr)
  (tlabel plstr))


(pl-defcfun ("c_pllightsource" pllightsource) :void 
    "Sets the 3D position of the light source."
  (x plflt)
  (y plflt)
  (z plflt))


(pl-defcfun ("c_plline" plline) :void
    "Draw a line."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt))


(pl-defcfun ("plMinMax2dGrid" plminmax2dgrid) :void
    "Find the minimum and maximum of a 2d grid allocated using plAlloc2dGrid."
  (f **plflt)
  (nx plint (array-dimension f 0) nil)
  (ny plint (array-dimension f 1) nil)
  (fmax *plflt 1)
  (fmin *plflt 1))


(pl-defcfun ("c_plmtex" plmtex) :void
    "Write text relative to viewport boundaries."
  (side plstr)
  (disp plflt)
  (pos plflt)
  (just plflt)
  (text plstr))


(pl-defcfun ("c_plmtex3" plmtex3) :void
    "Write text relative to viewport boundaries in 3D plots."
  (side plstr)
  (disp plflt)
  (pos plflt)
  (just plflt)
  (text plstr))


(pl-defcfun ("c_plpoin" plpoin) :void
    "Plot a glyph at the specified points."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt)
  (code plint))


(pl-defcfun ("c_plptex" plptex) :void
    "Write text inside the viewport."
  (x plflt)
  (y plflt)
  (dx plflt)
  (dy plflt)
  (just plflt)
  (text plstr))


(pl-defcfun ("c_plschr" plschr) :void
    "Set character size."
  (def plflt)
  (scale plflt))


(pl-defcfun ("c_plscmap0" plscmap0) :void 
    "Set color map0 colors by 8-bit RGB values."
  (r *plint)
  (g *plint)
  (b *plint)
  (ncol0 plint (length r) (= (length r) (length g) (length b))))


(pl-defcfun ("c_plscmap1l" plscmap1l) :void
    "Set color map1 colors using a piece-wise linear relationship."
  (itype plbool)
  (npts plint (length pos) (= (length pos) (length coord1) (length coord2) (length coord3)))
  (pos *plflt)
  (coord1 *plflt)
  (coord2 *plflt)
  (coord3 *plflt)
  (rev *plbool))


(pl-defcfun ("c_plscmap1n" plscmap1n) :void 
    "Set color map1 colors using a piece-wise linear relationship."
  (ncol1 plint))


(pl-defcfun ("c_plsdev" plsdev) :void 
    "Set the device (keyword) name."
  (dev plstr))


(pl-defcfun ("c_plsori" plsori) :void
    "Set orientation."
  (ori plint))


(pl-defcfun ("c_plssub" plssub) :void
    "Set the number of subpages in x and y."
  (nx plint)
  (ny plint))


(pl-defcfun ("c_plstar" plstar) :void 
    "Initialization."
  (nx plint)
  (ny plint))


(pl-defcfun ("c_plsurf3d" plsurf3d) :void 
    "Plot shaded 3-d surface plot."
  (x *plflt)
  (y *plflt)
  (z **plflt)
  (nx plint (length x) nil)
  (ny plint (length y) nil)
  (opt plint)
  (clevel *plflt)
  (nlevel plint (pl-length clevel) nil))


(pl-defcfun ("c_plsym" plsym) :void 
    "Plot a glyph at the specified points."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt)
  (code plint))


(pl-defcfun ("c_plstyl" plstyl) :void
    "Set line style."
  (nms plint nil (and (<= nms (length mark)) (<= nms (length space))))
  (mark *plint)
  (space *plint))


(pl-defcfun ("c_plvpor" plvpor) :void
    "Specify viewport using coordinates."
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt))


(pl-defcfun ("c_plvsta" plvsta) :void
    "Select standard viewport.")


(pl-defcfun ("c_plw3d" plw3d) :void 
    "Set up window for 3-d plotting."
  (basex plflt)
  (basey plflt)
  (height plflt)
  (xmin0 plflt)
  (xmax0 plflt)
  (ymin0 plflt)
  (ymax0 plflt)
  (zmin0 plflt)
  (zmax0 plflt)
  (alt plflt)
  (az plflt))


(pl-defcfun ("c_plwidth" plwidth) :void
    "Set pen width."
  (width plflt))


(pl-defcfun ("c_plwind" plwind) :void
    "Specify world coordinates of viewport boundaries."
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt))

;;;
;;; deprecated plplot API functions, also ordered alphabetically.
;;;

;; Don't break legacy code, at least for the time being, but provide a warning.
(defun plwid (width)
  "Deprecated, use plwidth()."
  (progn
    (format t "plwid() is deprecated, use plwidth() instead.")
    (plwidth width)))

;;;;
;;;; Copyright (c) 2014 Hazen P. Babcock
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;;; of this software and associated documentation files (the "Software"), to 
;;;; deal in the Software without restriction, including without limitation the 
;;;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
;;;; sell copies of the Software, and to permit persons to whom the Software is 
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in 
;;;; all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;;; IN THE SOFTWARE.
;;;;
