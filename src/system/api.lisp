;;;;
;;;; Calls to the plplot API.
;;;;
;;;; Doc-strings are from the api.xml file in PLplot.
;;;;
;;;; This only contains functions that were used in the PLplot examples, or in
;;;; the additional examples that are part of cl-plplot.
;;;;
;;;;
;;;; hazen 02/14
;;;;

(in-package #:cl-plplot-system)

;;;
;;; PLplot API functions, ordered alphabetically by PLplot function name, this 
;;; more or less follows the order that they are documented in the PLplot manual.
;;;

(pl-defcfun ("pl_cmd" pl-cmd) :void
    "Front end to driver escape function."
  (cmd plint)
  (ptr :pointer))

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


(pl-defcfun ("plAlloc2dGrid" plalloc2dgrid) :void 
    "Allocate a block of memory for use as a 2-d grid of type PLFLT."
  (f :pointer)
  (nx plint)
  (ny plint))


(pl-defcfun ("c_plarc" plarc) :void
    "Draw a circular or elliptical arc."
  (x plflt)
  (y plflt)
  (a plflt)
  (b plflt)
  (angle1 plflt)
  (angle2 plflt)
  (rotate plflt)
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


(pl-defcfun ("c_plbtime" plbtime) :void
    "Calculate broken-down time from continuous time for the current stream."
  (year *plint 1)
  (month *plint 1)
  (day *plint 1)
  (hour *plint 1)
  (min *plint 1)
  (sec *plflt 1)
  (ctime plflt))


(pl-defcfun ("c_plcalc_world" plcalc-world) :void
    "Calculate world coordinates and corresponding window index from relative device coordinates."
  (rx plflt)
  (ry plflt)
  (wx *plflt 1)
  (wy *plflt 1)
  (window *plint 1))


(pl-defcfun ("c_plcol0" plcol0) :void
    "Set color, map0."
  (icol0 plint))


(pl-defcfun ("c_plcol1" plcol1) :void
    "Set color, map1."
  (col1 plflt))


(pl-defcfun ("c_plcolorbar" plcolorbar) :void
    "Plot color bar for image, shade or gradient plots."
  (p_colorbar_width *plflt 1)
  (p_colorbar_height *plflt 1)
  (opt plint)
  (position plint)
  (x plflt)
  (y plflt)
  (x_length plflt)
  (y_length plflt)
  (bg_color plint)
  (bb_color plint)
  (bb_style plint)
  (low_cap_color plflt)
  (high_cap_color plflt)
  (cont_color plint)
  (cont_width plflt)
  (n_labels plint (length label_opts) (= (length label_opts) (length labels_text)))
  (label_opts *plint)
  (labels_text *plstr)
  (n_axes plint (length axis_opts) (= (length axis_opts) (length ticks) (length sub_ticks) (length n_values)))
  (axis_opts *plstr)
  (ticks *plflt)
  (sub_ticks *plint)
  (n_values *plint)
  (values_arr **plflt))


(pl-defcfun ("c_plconfigtime" plconfigtime) :void
    "Configure the transformation between continuous and broken-down time for the current stream."
  (scale plflt)
  (offset1 plflt)
  (offset2 plflt)
  (ccontrol plint)
  (ifbtime-offset plbool)
  (year plint)
  (month plint)
  (day plint)
  (hour plint)
  (min plint)
  (sec plflt))


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


(pl-defcfun ("c_plctime" plctime) :void
    "Calculate continuous time from broken-down time for the current stream."
  (year plint)
  (month plint)
  (day plint)
  (hour plint)
  (min plint)
  (sec plflt)
  (ctime *plflt 1))


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


(pl-defcfun ("c_plenv0" plenv0) :void
    "Same as plenv but if in multiplot mode does not advance the subpage, instead clears it."
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt)
  (just plint)
  (axis plint))


(pl-defcfun ("c_pleop" pleop) :void
    "Eject current page.")


(pl-defcfun ("c_plerrx" plerrx) :void
    "Draw x error bar."
  (n plint (length xmin) (= (length xmin) (length xmax) (length y)))
  (xmin *plflt)
  (xmax *plflt)
  (y *plflt))


(pl-defcfun ("c_plerry" plerry) :void
    "Draw y error bar."
  (n plint (length x) (= (length x) (length ymin) (length ymax)))
  (x *plflt)
  (ymin *plflt)
  (ymax *plflt))


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


(pl-defcfun ("plfcont" plfcont) :void
    "Undocumented."
  (feval plfunc)
  (f2eval-data pldata)
  (nx plint)
  (ny plint)
  (kx plint)
  (lx plint)
  (ky plint)
  (ly plint)
  (clevel *plflt)
  (nlevel plint (length clevel) nil)
  (pltr plfunc)
  (pltr-data pldata))


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


(pl-defcfun ("plFree2dGrid" plfree2dgrid) :void
    "Free the memory associated with a 2-d grid allocated using plAlloc2dGrid."
  (f :pointer)
  (nx plint)
  (ny plint))


;;
;; See x08l.lisp for an example of how to use this function.
;;
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


(pl-defcfun ("c_plgchr" plgchr) :void
    "Get character default height and current (scaled) height."
  (p_def *plflt 1)
  (p_ht *plflt 1))


(pl-defcfun ("c_plgcol0" plgcol0) :void 
    "Returns 8-bit RGB values for given color from color map0."
  (icol0 plint)
  (r *plint 1)
  (g *plint 1)
  (b *plint 1))


(pl-defcfun ("c_plgcol0a" plgcol0a) :void 
    "Returns 8-bit RGB values and double alpha transparency value for given color index from cmap0."
  (icol0 plint)
  (r *plint 1)
  (g *plint 1)
  (b *plint 1)
  (a *plflt 1))


(pl-defcfun ("c_plgcolbg" plgcolbg) :void
    "Returns the background color (cmap0[0]) by 8-bit RGB value."
  (r *plint 1)
  (g *plint 1)
  (b *plint 1))


(pl-defcfun ("c_plgcolbga" plgcolbga) :void
    "Returns the background color (cmap0[0]) by 8-bit RGB value and double alpha transparency value."
  (r *plint 1)
  (g *plint 1)
  (b *plint 1)
  (a *plflt 1))


(pl-defcfun ("c_plgcompression" plgcompression) :void
    "Get the current device-compression setting."
  (compression *plint 1))


(pl-defcfun ("c_plgdidev" plgdidev) :void
    "Get parameters that define current device-space window."
  (p_mar *plflt 1)
  (p_aspect *plflt 1)
  (p_jx *plflt 1)
  (p_jy *plflt 1))


;; This works in that it seems to return the correct X & Y coordinates of the mouse click (in pixels).
;; I'm not so sure about subwindow, but maybe I don't get that parameter.

(defun plgetcursor ()
  "Wait for graphics input event and translate to world coordinates."
  (with-foreign-object (ptr '(:struct plgraphicsin))
    (init-plgraphicsin ptr)
    (pl-plgetcursor ptr)
    (with-foreign-slots ((state keysym button subwindow pX pY dX dY wX wY) ptr (:struct plgraphicsin))
      (list state keysym button subwindow pX pY dX dY wX wY))))

(export 'plgetcursor (package-name *package*))

(defcfun ("plGetCursor" pl-plgetcursor) plint
  (gin :pointer))


(pl-defcfun ("c_plgfam" plgfam) :void 
    "Get family file parameters."
  (p_fam *plint 1)
  (p_num *plint 1)
  (p_bmax *plint 1))


(pl-defcfun ("c_plgfci" plgfci) :void
    "Get FCI (font characterization integer)."
  (pfci *plunicode 1))


(pl-defcfun ("c_plgdiori" plgdiori) :void
    "Get plot orientation."
  (p_rot *plflt 1))


(pl-defcfun ("c_plgdiplt" plgdiplt) :void
    "Get parameters that define current plot-space window."
  (p_xmin *plflt 1)
  (p_ymin *plflt 1)
  (p_xmax *plflt 1)
  (p_ymax *plflt 1))


(defcfun ("c_plgfnam" c-plgfnam) :void  
  (fnam :pointer))

(defun plgfnam ()
  "Get output file name."
  (with-foreign-string (version-string (make-array 80 :element-type 'character))
    (c-plgfnam version-string)
    (foreign-string-to-lisp version-string)))

(export 'plgfnam)


(pl-defcfun ("c_plgfont" plgfont) :void
    "Get family, style and weight of the current font."
  (p_family *plint 1)
  (p_style *plint 1)
  (p_weight *plint 1))


(pl-defcfun ("c_plglevel" plglevel) :void
    "Get the (current) run level."
  (p_level *plint 1))


(pl-defcfun ("c_plgpage" plgpage) :void
    "Get page parameters."
  (p_xp *plflt 1)
  (p_yp *plflt 1)
  (p_xleng *plint 1)
  (p_yleng *plint 1)
  (p_xoff *plint 1)
  (p_yoff *plint 1))


(pl-defcfun ("c_plgradient" plgradient) :void
    "Draw linear gradient inside polygon."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt)
  (angle plflt))


;;
;; A wrapper to make the PLplot function of the same name easier to use from Lisp.
;;
(defun plgriddata (x y z xg yg type data)
  "Grid data from irregularly sampled data."
  (let ((sx (length x))
	(sy (length y))
	(f (foreign-alloc :pointer)))
    (plalloc2dgrid f sx sy)
    (let ((zg (mem-aref f :pointer)))
      (pl-plgriddata x y z xg yg zg type data)
      (let ((lisp-mat (make-array (list (length xg) (length yg)) :initial-element 0.0d0 :element-type 'double-float)))
	(dotimes (x (length xg))
	  (dotimes (y (length yg))
	    (setf (aref lisp-mat x y) (mem-aref (mem-aref zg :pointer x) 'plflt y))))
	(plfree2dgrid zg sx sy)
	(foreign-free f)
	lisp-mat))))

(export 'plgriddata (package-name *package*))
    
(pl-defcfun ("c_plgriddata" pl-plgriddata) :void 
  "Grid data from irregularly sampled data (unwrapped version)."
  (x *plflt)
  (y *plflt)
  (z *plflt)
  (npts plint (length x) (= (length x) (length y) (length z)))
  (xg *plflt)
  (nptsx plint (length xg) nil)
  (yg *plflt)
  (nptsy plint (length yg) nil)
  (zg :pointer)
  (type plint)
  (data plflt))


(pl-defcfun ("c_plgspa" plgspa) :void
    "Get current subpage parameters."
  (xmin *plflt 1)
  (xmax *plflt 1)
  (ymin *plflt 1)
  (ymax *plflt 1))


(defcfun ("c_plgver" c-plgver) :void
  (p_ver :pointer))

(defun plgver ()
  "Get the current library version number."
  (with-foreign-string (version-string (make-array 80 :element-type 'character))
    (c-plgver version-string)
    (foreign-string-to-lisp version-string)))

(export 'plgver)


(pl-defcfun ("c_plgvpd" plgvpd) :void
    "Get viewport limits in normalized device coordinates."
  (p_xmin *plflt 1)
  (p_xmax *plflt 1)
  (p_ymin *plflt 1)
  (p_ymax *plflt 1))


(pl-defcfun ("c_plgvpw" plgvpw) :void
    "Get viewport limits in world coordinates."
  (p_xmin *plflt 1)
  (p_xmax *plflt 1)
  (p_ymin *plflt 1)
  (p_ymax *plflt 1))


(pl-defcfun ("c_plgxax" plgxax) :void
    "Get x axis parameters."
  (p_digmax *plint 1)
  (p_digits *plint 1))


(pl-defcfun ("c_plgyax" plgyax) :void
    "Get y axis parameters."
  (p_digmax *plint 1)
  (p_digits *plint 1))


(pl-defcfun ("c_plgzax" plgzax) :void 
    "Get z axis parameters."
  (p_digmax *plint 1)
  (p_digits *plint 1))


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


(pl-defcfun ("c_pllegend" pllegend) :void
    "Plot legend using discretely annotated filled boxes, lines, and/or lines of symbols."
  (p_legend_width *plflt 1)
  (p_legend_height *plflt 1)
  (opt plint)
  (position plint)
  (x plflt)
  (y plflt)
  (plot_width plflt)
  (bg_color plint)
  (bb_color plint)
  (bb_style plint)
  (nrow plint)
  (ncolumn plint)
  (nlegend plint (length opt_array) (<= (length opt_array) (length text_colors) (length text)))
  (opt_array *plint)
  (text_offset plflt)
  (text_scale plflt)
  (text_spacing plflt)
  (text_justification plflt)
  (text_colors *plint)
  (text *plstr)
  (box_colors *plint)
  (box_patterns *plint)
  (box_scales *plflt)
  (box_line_widths *plflt)
  (line_colors *plint)
  (line_styles *plint)
  (line_widths *plflt)
  (symbol_colors *plint)
  (symbol_scales *plflt)
  (symbol_numbers *plint)
  (symbols *plstr))


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


(pl-defcfun ("c_plmapfill" plmapfill) :void
    "Plot all or a subset of Shapefile data, filling the polygons."
  (mapform plfunc)
  (name plstr)
  (minx plflt)
  (maxx plflt)
  (miny plflt)
  (maxy plflt)
  (plotentries *plint)
  (n plint (length plotentries) nil))


(pl-defcfun ("c_plmapline" plmapline) :void
    "Plot all or a subset of Shapefile data using lines in world coordinates."
  (mapform plfunc)
  (name plstr)
  (minx plflt)
  (maxx plflt)
  (miny plflt)
  (maxy plflt)
  (plotentries *plint)
  (n plint (length plotentries) nil))


(pl-defcfun ("c_plmapstring" plmapstring) :void
    "Plot all or a subset of Shapefile data using strings or points in world coordinates."
  (map-form plfunc)
  (name plstr)
  (string plstr)
  (minx plflt)
  (maxx plflt)
  (miny plflt)
  (maxy plflt)
  (plotentries *plint)
  (n plint (length plotentries) nil))


(pl-defcfun ("c_plmaptex" plmaptex) :void
    "Draw text at points defined by Shapefile data in world coordinates."
  (map-form plfunc)
  (name plstr)
  (dx plflt)
  (dy plflt)
  (just plflt)
  (text plstr)
  (minx plflt)
  (maxx plflt)
  (miny plflt)
  (maxy plflt)
  (plotentry plint))


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


(pl-defcfun ("c_plot3dc" plot3dc) :void
    "Magnitude colored plot surface with contour."
  (x *plflt)
  (y *plflt)
  (z **plflt)
  (nx plint (length x) (= (length x) (array-dimension z 0)))
  (ny plint (length y) (= (length y) (array-dimension z 1)))
  (opt plint)
  (clevel *plflt)
  (nlevel plint (length clevel) nil))


(pl-defcfun ("c_plpat" plpat) :void
    "Set area fill pattern."
  (nlin plint (length inc) (= (length inc) (length del)))
  (inc *plint)
  (del *plint))


(pl-defcfun ("c_plpath" plpath) :void
    "Draw a line between two points, accounting for coordinate transforms."
  (n plint)
  (x1 plflt)
  (y1 plflt)
  (x2 plflt)
  (y2 plflt))


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


(pl-defcfun ("c_plprec" plprec) :void
    "Set precision in numeric labels."
  (setp plint)
  (prec plint))


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


(pl-defcfun ("c_plptex3" plptex3) :void
    "Write text inside the viewport of a 3D plot."
  (x plflt)
  (y plflt)
  (z plflt)
  (dx plflt)
  (dy plflt)
  (dz plflt)
  (sx plflt)
  (sy plflt)
  (sz plflt)
  (just plflt)
  (text plstr))


(pl-defcfun ("c_plrandd" plrandd) :double
    "Random number generator returning a real random number in the range [0,1].")


(pl-defcfun ("c_plschr" plschr) :void
    "Set character size."
  (def plflt)
  (scale plflt))


(pl-defcfun ("c_plscol0" plscol0) :void
    "Set 8-bit RGB values for given cmap0 color index."
  (icol0 plint)
  (r plint)
  (g plint)
  (b plint))


(pl-defcfun ("c_plscolbg" plscolbg) :void
    "Set the background color by 8-bit RGB value."
  (r plint)
  (g plint)
  (b plint))


(pl-defcfun ("c_plscolbga" plscolbga) :void
    "Set the background color by 8-bit RGB value and double alpha transparency value."
  (r plint)
  (g plint)
  (b plint)
  (a plflt))


(pl-defcfun ("c_plscolor" plscolor) :void
    "Used to globally turn color output on/off."
  (color plint))


(pl-defcfun ("c_plscmap0" plscmap0) :void 
    "Set color map0 colors by 8-bit RGB values."
  (r *plint)
  (g *plint)
  (b *plint)
  (ncol0 plint (length r) (= (length r) (length g) (length b))))


(pl-defcfun ("c_plscmap0n" plscmap0n) :void
    "Set number of colors in color map0."
  (ncol0 plint))


(pl-defcfun ("c_plscmap0a" plscmap0a) :void
    "Set cmap0 colors by 8-bit RGB values and double alpha transparency value."
  (r *plint)
  (g *plint)
  (b *plint)
  (a *plflt)
  (ncol0 plint (length r) (= (length r) (length g) (length b) (length a))))


(pl-defcfun ("c_plscmap1" plscmap1) :void
    "Set cmap1 colors using 8-bit RGB values."
  (r *plint)
  (g *plint)
  (b *plint)
  (ncol1 plint (length r) (= (length r) (length g) (length b))))

(pl-defcfun ("c_plscmap1_range" plscmap1-range) :void
    "Set the cmap1 argument range for continuous color plots."
  (min-color plflt)
  (max-color plflt))

(pl-defcfun ("c_plscmap1a" plscmap1a) :void
    "Set cmap1 colors using 8-bit RGB values and double alpha transparency values."
  (r *plint)
  (g *plint)
  (b *plint)
  (a *plflt)
  (ncol1 plint (length r) (= (length r) (length g) (length b) (length a))))


(pl-defcfun ("c_plscmap1l" plscmap1l) :void
    "Set color map1 colors using a piece-wise linear relationship."
  (itype plbool)
  (npts plint (length pos) (= (length pos) (length coord1) (length coord2) (length coord3)))
  (pos *plflt)
  (coord1 *plflt)
  (coord2 *plflt)
  (coord3 *plflt)
  (rev *plbool))


(pl-defcfun ("c_plscmap1la" plscmap1la) :void
    "Set cmap1 colors and alpha transparency using a piece-wise linear relationship."
  (itype plbool)
  (npts plint (length intensity) (= (length intensity) (length coord1) (length coord2) (length coord3) (length coord4)))
  (intensity *plflt)
  (coord1 *plflt)
  (coord2 *plflt)
  (coord3 *plflt)
  (coord4 *plflt)
  (rev *plbool))


(pl-defcfun ("c_plscmap1n" plscmap1n) :void 
    "Set color map1 colors using a piece-wise linear relationship."
  (ncol1 plint))


(pl-defcfun ("c_plscol0a" plscol0a) :void
    "Set 8-bit RGB values and double alpha transparency value for given cmap0 color index."
  (icol0 plint)
  (r plint)
  (g plint)
  (b plint)
  (a plflt))


(pl-defcfun ("c_plscompression" plscompression) :void
    "Set device-compression level."
  (compression plint))


(pl-defcfun ("c_plsdev" plsdev) :void 
    "Set the device (keyword) name."
  (dev plstr))


(pl-defcfun ("c_plsdidev" plsdidev) :void
    "Set parameters that define current device-space window."
  (mar plflt)
  (aspect plflt)
  (jx plflt)
  (jy plflt))


(pl-defcfun ("c_plsdiori" plsdiori) :void
    "Set plot orientation."
  (rot plflt))


(pl-defcfun ("c_plsdiplt" plsdiplt) :void
    "Set parameters that define current plot-space window."
  (xmin plflt)
  (ymin plflt)
  (xmax plflt)
  (ymax plflt))


(pl-defcfun ("c_plsdiplz" plsdiplz) :void
    "Set parameters incrementally (zoom mode) that define current plot-space window."
  (xmin plflt)
  (ymin plflt)
  (xmax plflt)
  (ymax plflt))


(pl-defcfun ("c_plsesc" plsesc) :void
    "Set the escape character for text strings."
  (esc plchar))


(pl-defcfun ("c_plsetopt" plsetopt) :int
    "Set any command-line option."
  (opt plstr)
  (optarg plstr))


(pl-defcfun ("c_plseed" plseed) :void
    "Set seed for internal random number generator."
  (seed plint))


;; We use plsexit to set our own error handler that will
;; throw a Lisp side error and keep PLplot from calling exit().
(defcallback trap-plsexit :int ((message plstr))
  (error "PLplot error encountered (~A). The current plotting stream is likely corrupted.~%"
	 (foreign-string-to-lisp message)))

(pl-defcfun ("plsexit" plsexit) :void
    "Set exit handler."
  (plsexit-fn plfunc))

(plsexit 'trap-plsexit)


(pl-defcfun ("c_plsfam" plsfam) :void
    "Set family file parameters."
  (fam plint)
  (num plint)
  (bmax plint))


(pl-defcfun ("c_plsfci" plsfci) :void
    "Set FCI (font characterization integer)."
  (fci plunicode))


(pl-defcfun ("c_plsfont" plsfont) :void 
    "Set family, style and weight of the current font."
  (family plint)
  (style plint)
  (weight plint))


;; This is inside a closure so that the file name & its associated storage
;; is preserved until the next time this function is called

(let ((c-fname nil))
  (defun plsfnam (fname)
    (when c-fname
      (foreign-string-free c-fname))
    (setf c-fname (foreign-string-alloc fname))
    (c-plsfnam c-fname)))

(export 'plsfnam)

(defcfun ("c_plsfnam" c-plsfnam) :void 
  (fnam plstr))


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
;; From a C perspective it is different as the 2D lisp array is passed
;; in as a 1D C array.
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


(pl-defcfun ("c_plsmaj" plsmaj) :void
    "Set length of major ticks."
  (def plflt)
  (scale plflt))


(pl-defcfun ("c_plsmin" plsmin) :void
    "Set length of minor ticks."
  (def plflt)
  (scale plflt))


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


(pl-defcfun ("c_plspage" plspage) :void
    "Set page parameters."
  (xp plflt)
  (yp plflt)
  (xleng plint)
  (yleng plint)
  (xoff plint)
  (yoff plint))


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


(pl-defcfun ("c_plstring" plstring) :void
    "Plot a glyph at the specified points."
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt)
  (glyph plstr))


(pl-defcfun ("c_plstring3" plstring3) :void
    "Plot a glyph at the specified 3D points"
  (n plint (length x) (= (length x) (length y)))
  (x *plflt)
  (y *plflt)
  (z *plflt)
  (glyph plstr))


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


(pl-defcfun ("c_plsurf3dl" plsurf3dl) :void 
    "Plot shaded 3-d surface plot for z[x][y] with y index limits."
  (x *plflt)
  (y *plflt)
  (z **plflt)
  (nx plint (length x) nil)
  (ny plint (length y) nil)
  (opt plint)
  (clevel *plflt)
  (nlevel plint (pl-length clevel) nil)
  (indexxmin plint)
  (indexxmax plint)
  (indexymin *plint)
  (indexymax *plint))


(pl-defcfun ("c_plsvect" plsvect) :void
    "Set arrow style for vector plots."
  (arrowx *plflt)
  (arrowy *plflt)
  (npts plint (length arrowx) (= (length arrowx) (length arrowy)))
  (fill plint))


(pl-defcfun ("c_plsvpa" plsvpa) :void
    "Specify viewport in absolute coordinates."
  (xmin plflt)
  (xmax plflt)
  (ymin plflt)
  (ymax plflt))


(pl-defcfun ("c_plsxax" plsxax) :void
    "Set x axis parameters."
  (digmax plint)
  (digits plint))


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


(pl-defcfun ("c_plszax" plszax) :void
    "Set z axis parameters."
  (digmax plint)
  (digits plint))


(pl-defcfun ("c_pltimefmt" pltimefmt) :void
    "Set format for date / time labels."
  (fmt plstr))


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


(pl-defcfun ("c_plvect" plvect) :void
    "Vector plot."
  (u **plflt)
  (v **plflt)
  (nx plint (array-dimension u 0) (= (array-dimension u 0) (array-dimension v 0)))
  (ny plint (array-dimension u 1) (= (array-dimension u 1) (array-dimension v 1)))
  (scale plflt)
  (pltr plfunc)
  (pltr-data pldata))


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
;;; Deprecated plplot API functions, also ordered alphabetically.
;;;

;; Don't break legacy code, at least for the time being, but provide a warning.
(defun plwid (width)
  "Deprecated, use plwidth()."
  (progn
    (format t "plwid() is deprecated, use plwidth() instead.~%")
    (plwidth width)))

(export 'plwid)

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
