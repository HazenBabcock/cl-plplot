;;;;
;;;; Calls to the plplot API.
;;;;
;;;; Doc-strings are from the api.xml file in PLplot.
;;;;
;;;; hazen 01/14
;;;;

(in-package #:cl-plplot-system)

;;;
;;; PLplot API functions, ordered alphabetically by PLplot function name, this 
;;; more or less follows the order that they are documented in the PLplot manual.
;;;

(pl-defcfun ("c_pl_setcontlabelformat" pl-setcontlabelformat) :void
    "Set format of numerical label for contours."
  (lexp plint)
  (sigdig plint))


(pl-defcfun ("c_pl_setcontlabelparam" pl-setcontlabelparam) :void
    "Set parameters of contour labelling other than format of numerical label."
  (offset plflt)
  (size plflt)
  (spacing plflt)
  (active plint))


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


(pl-defcfun ("c_plcol1" plcol1) :void
    "Set color, map1."
  (col1 plflt))


(pl-defcfun ("c_plcont" plcont) :void
    "Contour plot."
  (z **plflt)
  (nx plint (array-dimension z 0) nil)
  (ny plint (array-dimension z 1) nil)
  (kx plint)
  (lx plint)
  (ky plint)
  (ly plint)
  (clevel *plflt)
  (nlevel plint (pl-length clevel) nil)
  (pltr plfunc)
  (pltr-data pldata))


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


(pl-defcfun ("c_plfill" plfill) :void
    "Draw filled polygon.
     Also available as a callback (plfill-callback)."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt))

(pl-callback (c-plfill plfill-callback) :void
  (n plint)
  (x *plflt)
  (y *plflt))


(pl-defcfun ("c_plfill3" plfill3) :void
    "Draw filled polygon in 3D."
  (n plint (length x) (= (length x) (length y) (length z)))
  (x *plflt)
  (y *plflt)
  (z *plflt))


(pl-defcfun ("c_plflush" plflush) :void
    "Flushes the output stream.")


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


(pl-defcfun ("c_plgfam" plgfam) :void 
    "Get family file parameters."
  (p_fam *plint 1)
  (p_num *plint 1)
  (p_bmax *plint 1))


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


(pl-defcfun ("c_plimagefr" plimagefr) :void
    "Plot a 2D matrix using color map1."
  (idata **plflt)
  (nx plint (array-dimension idata 0) nil)
  (ny plint (array-dimension idata 1) nil)
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt)
  (zmin plflt)
  (zmax plflt)
  (valuemin plflt)
  (valuemax plflt)
  (pltr plfunc)
  (pltr-data pldata))


(pl-defcfun ("c_plimage" plimage) :void
    "Plot a 2D matrix using color map1 with automatic colour adjustment."
  (data **plflt)
  (nx plint (array-dimension data 0) nil)
  (ny plint (array-dimension data 1) nil)
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt)
  (zmin plflt)
  (zmax plflt)
  (Dxmin plflt)
  (Dxmax plflt)
  (Dymin plflt)
  (Dymax plflt))


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


(pl-defcfun ("c_plline3" plline3) :void
    "Draw a line in 3 space."
  (n plint (length x) (= (length x) (length y) (length z)))
  (x *plflt)
  (y *plflt)
  (z *plflt))


(pl-defcfun ("c_pllsty" pllsty) :void
    "Select line style."
  (lin plint))


(pl-defcfun ("c_plmap" plmap) :void
    "Plot continental outline in world coordinates."
  (map-form plfunc)
  (type plstr)
  (minlong plflt)
  (maxlong plflt)
  (minlat plflt)
  (maxlat plflt))


(pl-defcfun ("c_plmeridians" plmeridians) :void
    "Plot latitude and longitude lines."
  (map-form plfunc)
  (dlong plflt)
  (dlat plflt)
  (minlong plflt)
  (maxlong plflt)
  (minlat plflt)
  (maxlat plflt))


(pl-defcfun ("c_plmesh" plmesh) :void
    "Plot surface mesh."
  (x *plflt)
  (y *plflt)
  (z **plflt)
  (nx plint (length x) (= (length x) (array-dimension z 0)))
  (ny plint (length y) (= (length y) (array-dimension z 1)))
  (opt plint))


(pl-defcfun ("c_plmeshc" plmeshc) :void
    "Magnitude colored plot surface mesh with contour."
  (x *plflt)
  (y *plflt)
  (z **plflt)
  (nx plint (length x) (= (length x) (array-dimension z 0)))
  (ny plint (length y) (= (length y) (array-dimension z 1)))
  (opt plint)
  (clevel *plflt)
  (nlevel plint (pl-length clevel) nil))


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


(pl-defcfun ("c_plot3d" plot3d) :void
    "Plot 3-d surface plot."
  (x *plflt)
  (y *plflt)
  (z **plflt)
  (nx plint (length x) (= (length x) (array-dimension z 0)))
  (ny plint (length y) (= (length y) (array-dimension z 1)))
  (opt plint)
  (side plint))


(pl-defcfun ("c_plpat" plpat) :void
    "Set area fill pattern."
  (nlin plint (length inc) (= (length inc) (length del)))
  (inc *plint)
  (del *plint))


(pl-defcfun ("c_plpoin" plpoin) :void
    "Plot a glyph at the specified points."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt)
  (code plint))


(pl-defcfun ("c_plpoin3" plpoin3) :void 
    "Plot a glyph at the specified 3D points."
  (n plint (length x) (= (length x) (length y) (length z)))
  (x *plflt)
  (y *plflt)
  (z *plflt)
  (code plint))


(pl-defcfun ("c_plpoly3" plpoly3) :void
    "Draw a polygon in 3 space."
  (n plint (length x) (= (length x) (length y) (length z) (1+ (length draw))))
  (x *plflt)
  (y *plflt)
  (z *plflt)
  (draw *plint)
  (ifcc plbool))


(pl-defcfun ("c_plpsty" plpsty) :void
    "Select area fill pattern."
  (patt plint))


(pl-defcfun ("c_plptex" plptex) :void
    "Write text inside the viewport."
  (x plflt)
  (y plflt)
  (dx plflt)
  (dy plflt)
  (just plflt)
  (text plstr))


(pl-defcfun ("c_plrandd" plrandd) :double
    "Random number generator returning a real random number in the range [0,1].")


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


(pl-defcfun ("c_plscmap0n" plscmap0n) :void
    "Set number of colors in color map0."
  (ncol0 plint))


(pl-defcfun ("c_plscmap1n" plscmap1n) :void 
    "Set color map1 colors using a piece-wise linear relationship."
  (ncol1 plint))


(pl-defcfun ("c_plsdev" plsdev) :void 
    "Set the device (keyword) name."
  (dev plstr))


(pl-defcfun ("c_plsetopt" plsetopt) :int
    "Set any command-line option."
  (opt plstr)
  (optarg plstr))


(pl-defcfun ("c_plsfam" plsfam) :void
    "Set family file parameters."
  (fam plint)
  (num plint)
  (bmax plint))


(pl-defcfun ("c_plshades" plshades) :void
    "Shade regions on the basis of value."
  (a **plflt)
  (nx plint (array-dimension a 0) nil)
  (ny plint (array-dimension a 1) nil)
  (defined plfunc)
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt)
  (clevel *plflt)
  (nlevel plint (pl-length clevel) nil)
  (fill-width plflt)
  (cont-color plint)
  (cont-width plflt)
  (plfill plfunc)
  (rectangular plbool)
  (pltr plfunc)
  (pltr-data pldata))


(pl-defcfun ("c_plshade" plshade) :void
    "Shade individual region on the basis of value."
  (a **plflt)
  (nx plint (array-dimension a 0) nil)
  (ny plint (array-dimension a 1) nil)
  (defined plfunc)
  (left plflt)
  (right plflt)
  (bottom plflt)
  (top plflt)
  (shade-min plflt)
  (shade-max plflt)
  (sh-cmap plint)
  (sh-color plflt)
  (sh-width plflt)
  (min-color plint)
  (min-width plflt)
  (max-color plint)
  (max-width plflt)
  (plfill plfunc)
  (rectangular plbool)
  (pltr plfunc)
  (pltr-data pldata))


;;
;; From the perspective of Lisp this function is identical to the plshade.
;; From a C perspective it is different as the 2D lisp array comes is
;; passed as 1D C array.
;;
(pl-defcfun ("c_plshade1" plshade1) :void
    "Shade individual region on the basis of value."
  (a *plflt)
  (nx plint (array-dimension a 0) nil)
  (ny plint (array-dimension a 1) nil)
  (defined plfunc)
  (left plflt)
  (right plflt)
  (bottom plflt)
  (top plflt)
  (shade-min plflt)
  (shade-max plflt)
  (sh-cmap plint)
  (sh-color plflt)
  (sh-width plflt)
  (min-color plint)
  (min-width plflt)
  (max-color plint)
  (max-width plflt)
  (plfill plfunc)
  (rectangular plbool)
  (pltr plfunc)
  (pltr-data pldata))


(pl-defcfun ("c_plslabelfunc" plslabelfunc) :void
    "Assign a function to use for generating custom axis labels."
  (label-func plfunc)
  (label-data pldata))


(pl-defcfun ("c_plsori" plsori) :void
    "Set orientation."
  (ori plint))


(pl-defcfun ("c_plspal0" plspal0) :void
    "Set the colors for color table 0 from a cmap0 file."
  (filename plstr))


(pl-defcfun ("c_plspal1" plspal1) :void
    "Set the colors for color table 1 from a cmap1 file."
  (filename plstr)
  (interpolate plbool))


(pl-defcfun ("c_plspause" plspause) :void
    "Set the pause (on end-of-page) status."
  (pause plbool))


(pl-defcfun ("c_plsstrm" plsstrm) :void
    "Set current output stream."
  (strm plint))


(pl-defcfun ("c_plssub" plssub) :void
    "Set the number of subpages in x and y."
  (nx plint)
  (ny plint))


(pl-defcfun ("c_plssym" plssym) :void 
    "Set symbol size."
  (def plflt)
  (scale plflt))


(pl-defcfun ("c_plstar" plstar) :void 
    "Initialization."
  (nx plint)
  (ny plint))


(pl-defcfun ("c_plstransform" plstransform) :void
    "Set a global coordinate transform function."
  (transform-fun plfunc)
  (data pldata))


(pl-defcfun ("c_plstripa" plstripa) :void
    "Add a point to a strip chart."
  (id plint)
  (pen plint)
  (x plflt)
  (y plflt))


;;
;; Expects colline, styline & legline to 4 element lists containing the
;; appropriate color, style and name for each pen
;;
(defun plstripc (xspec yspec xmin xmax xjump ymin ymax xlpos ylpos y_ascl acc colbox collab colline styline legline labx laby labtop)
  "Create a 4-pen strip chart."
  (let ((c-col (foreign-alloc 'plint :count 4))
	(c-sty (foreign-alloc 'plint :count 4))
	(c-leg (foreign-alloc :pointer :count 4)))
    (dotimes (i 4)
      (setf (mem-aref c-col 'plint i) (round (elt colline i)))
      (setf (mem-aref c-sty 'plint i) (round (elt styline i)))
      (setf (mem-aref c-leg :pointer i) (foreign-string-alloc (elt legline i))))
    (unwind-protect
	 (pl-plstripc xspec yspec xmin xmax xjump ymin ymax xlpos ylpos y_ascl acc colbox collab c-col c-sty c-leg labx laby labtop)
      (progn
	(foreign-free c-col)
	(foreign-free c-sty)
	(dotimes (i 4)
	  (foreign-string-free (mem-aref c-leg :pointer i)))
	(foreign-free c-leg)))))

(export 'plstripc (package-name *package*))

(pl-defcfun ("c_plstripc" pl-plstripc) :void
    "Create a 4-pen strip chart."
  (id *plint 1)
  (xspec plstr)
  (yspec plstr)
  (xmin plflt)
  (xmax plflt)
  (xjump plflt)
  (ymin plflt)
  (ymax plflt)
  (xlpos plflt)
  (ylpos plflt)
  (y_ascl plbool)
  (acc plbool)
  (colbox plint)
  (collab plint)
  (colline :pointer)
  (styline :pointer)
  (legline :pointer)
  (labx plstr)
  (laby plstr)
  (labtop plstr))


(pl-defcfun ("c_plstripd" plstripd) :void
    "Deletes and releases memory used by a strip chart."
  (id plint))


(pl-defcfun ("c_plstyl" plstyl) :void
    "Set line style."
  (nms plint nil (and (<= nms (length mark)) (<= nms (length space))))
  (mark *plint)
  (space *plint))


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


(pl-defcfun ("c_plsvpa" plsvpa) :void
    "Specify viewport in absolute coordinates."
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt))


(pl-defcfun ("c_plsyax" plsyax) :void
    "Set y axis parameters."
  (digmax plint)
  (digits plint))


(pl-defcfun ("c_plsym" plsym) :void 
    "Plot a glyph at the specified points."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt)
  (code plint))



;; The pltr series functions are designed to be passed as callback arguments to other plplot 
;; functions like the shade functions. pltr1 & pltr2 let you interpolate your data onto other
;; non-rectangular grids, but you have to pass them the appropriate information via the
;; pltr_data pointer or they will exit & take down lisp in the process. They are used
;; in example9.lisp.

(pl-defcfun ("pltr0" pltr0) :void
    "Identity transformation for grid to world mapping.
     Also available as a callback (pltr0-callback)."
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr-data :pointer))

(pl-callback (pltr0 pltr0-callback) :void
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr-data :pointer))


(pl-defcfun ("pltr1" pltr1) :void 
    "Linear interpolation for grid to world mapping using singly dimensioned coordinate arrays.
     Also available as a callback (pltr1-callback)."
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr-data :pointer))

(pl-callback (pltr1 pltr1-callback) :void
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr-data :pointer))


(pl-defcfun ("pltr2" pltr2) :void
    "Linear interpolation for grid to world mapping using doubly dimensioned coordinate arrays 
     (column dominant, as per normal C 2d arrays).
     Also available as a callback (pltr2-callback)."
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr-data :pointer))

(pl-callback (pltr2 pltr2-callback) :void
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr-data :pointer))


; This in plplot.h but is not documented.
(pl-defcfun ("pltr2p" pltr2p) :void
    "Undocumented."
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr-data :pointer))


(pl-defcfun ("c_plvasp" plvasp) :void
    "Specify viewport using aspect ratio only."
  (aspect plflt))


(pl-defcfun ("c_plvpas" plvpas) :void
    "Specify viewport using coordinates and aspect ratio."
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt)
  (aspect plflt))


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
