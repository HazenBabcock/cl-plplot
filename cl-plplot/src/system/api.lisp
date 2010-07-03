;;;;
;;;; Calls to the plplot API. An attempt was made to have the pl-defcfun macro
;;;; handle as many as these as possible, but eventually I gave up and just wrote 
;;;; out some of the less conventional ones. As a consequence, not all the 'raw'
;;;; cffi calls will be name "c-plfunctionname", they are likely to instead be
;;;; "c-pl-plfunctionname".
;;;;
;;;; hazen 3/06
;;;;

(in-package #:cl-plplot-system)


;;;
;;; plplot API functions, these are in more or less the same order as they are listed in plplot.h
;;;

(pl-defcfun ("c_pl_setcontlabelformat" pl-setcontlabelformat) :void 
	    (lexp plint)
	    (sigdig plint))

(pl-defcfun ("c_pl_setcontlabelparam" pl-setcontlabelparam) :void
	    (offset plflt)
	    (size plflt)
	    (spacing plflt)
	    (active plint))

(pl-defcfun ("c_pladv" pladv) :void 
	    (page plint))

(pl-defcfun ("c_plarc" plarc) :void
	    (x plflt)
	    (y plflt)
	    (a plflt)
	    (b plflt)
	    (angle1 plflt)
	    (angle2 plflt)
	    (fill plbool))
	    
;;
;; This function is deprecated and may dissappear at some point
;;

(pl-defcfun ("plarrows" plarrows) :void
	    (u *plflt n)
	    (v *plflt n)
	    (x *plflt n)
	    (y *plflt n)
	    (n plint)
	    (scale plflt)
	    (dx plflt)
	    (dy plflt))

(defun plvect (u v scale &optional (gridx nil) (gridy nil))
  (with-plcgrid (plc-grid gridx gridy (array-dimension u 0) (array-dimension u 1))  ; this macro creates the variable plcgrid
    (pl-plvect u v scale plc-grid)))

(export 'plvect (package-name *package*))

(pl-defcfun ("c_plvect" pl-plvect) :void
	    (u **plflt (nx ny))
	    (v **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (scale plflt)
	    (pltr-fn plfunc)
	    (pltr-data plpointer))

(pl-defcfun ("c_plsvect" plsvect) :void 
	(arrowx *plflt npts)
	(arrowy *plflt npts)
	(npts plint)
	(fill plint))

(pl-defcfun ("c_plaxes" plaxes) :void 
	(x0 plflt)
	(y0 plflt)
	(xopt plstr)
	(xtick plflt)
	(nxsub plint)
	(yopt plstr)
	(ytick plflt)
	(nysub plint))

(pl-defcfun ("c_plbin" plbin) :void 
	(nbin plint)
	(x *plflt nbin)
	(y *plflt nbin)
	(opt plint))

(pl-defcfun ("c_plbop" plbop) :void)

(pl-defcfun ("c_plbox" plbox) :void 
	    (xopt plstr)
	    (xtick plflt)
	    (nxsub plint)
	    (yopt plstr)
	    (ytick plflt)
	    (nysub plint))

(pl-defcfun ("c_plbox3" plbox3) :void 
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

(pl-defcfun ("c_plcalc_world" plcalc-world) :void 
	    (rx plflt)
	    (ry plflt)
	    (wx *plflt 1)
	    (wy *plflt 1)
	    (window *plint 1))

(pl-defcfun ("c_plclear" plclear) :void)

(pl-defcfun ("c_plcol0" plcol0) :void 
	    (icol0 plint))

(pl-defcfun ("c_plcol1" plcol1) :void 
	    (col1 plflt))


;;
;; Coming on this a couple months later I had a pretty hard time figuring
;; out what I was trying to do in the first place, so here I attempt a
;; better explanation.
;;
;; In the case of 2D gridded data, as you might have for a contour or
;; surface plot, plplot allows you do one of four things to specify
;; how to map that data to world (i.e. plot coordinates).
;;
;; (1) Nothing, meaning matrix element (0,0) goes to coordinate (0,0).
;;      This is done with the plplot function pltr0.
;; (2) Map to a grid defined by a vector in x and a vector in y
;;     (0,0) -> (x(0), y(0)), handled by the function pltr1.
;; (3) Map to a grid defined my a matrix in x and y.
;;     (0,0) -> (x(0,0), y(0,0)), handled by the function pltr2.
;; (4) Completely arbitrary. This is implemented here by passing the 
;;     data contained in gridx through C to a callback function that 
;;     you specify "on the other side". Since the "other side" is back
;;     where you started, i.e. in Lisp, you also have the option of 
;;     using in your callback function anything that it is accessible
;;     to it in the current Lisp environment.
;;
;; The functions pltr0, pltr1 & pltr2, along with the relevant callback
;; closure, pltr-fn, are defined towards the end of this file.
;;
;; The macro callback-closure creates a closure containing the callback
;; function as well as getter, setter and query functions for
;; changing and querying the current callback function.
;;
;; If you want to use your own callback function it *must* take the
;; same arguments as the default function(s) for the particular callback
;; closure.
;;

(defun plcont (f kx lx ky ly clevel &optional (gridx nil) (gridy nil))
  (with-plcgrid (plc-grid gridx gridy (array-dimension f 0) (array-dimension f 1))
    (pl-plcont f kx lx ky ly clevel plc-grid)))

(export 'plcont (package-name *package*))

(pl-defcfun ("c_plcont" pl-plcont) :void
	    (f **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (kx plint)
	    (lx plint)
	    (ky plint)
	    (ly plint)
	    (clevel *plflt nlevel)
	    (nlevel plint)
	    (pltr-fn plfunc)
	    (pltr-data plpointer))


;; Draws the completely arbitrary in every was contour plot.
;; Useful for making contour plots of user defined functions.

(defun init-f2eval-data (f2eval-data nx ny)
  "Create C version of f2eval-data, if necessary"
  (if (equal (pl-get-feval-fn) #'plf2eval2)
      (if (and (= (array-dimension f2eval-data 0) nx)
	       (= (array-dimension f2eval-data 1) ny))
	  (make-matrix f2eval-data)
	  (format t "Matrix dimensions are wrong for f2eval-data in init-f2eval-data~%"))
      f2eval-data))

(defun free-f2eval-data (f2eval-data nx ny)
  "Frees C version of f2eval-data, if necessary"
  (when (equal (pl-get-feval-fn) #'plf2eval2)
    (free-matrix f2eval-data (list nx ny))))

(defun plfcont (f2eval-data nx ny kx lx ky ly clevel &optional (gridx nil) (gridy nil))
  (let ((c-f2eval-data (init-f2eval-data f2eval-data nx ny)))
    (with-plcgrid (plc-grid gridx gridy nx ny)
      (pl-plfcont c-f2eval-data nx ny kx lx ky ly clevel plc-grid))
    (free-f2eval-data f2eval-data nx ny)))

(export 'plfcont (package-name *package*))

(pl-defcfun ("plfcont" pl-plfcont) :void 
	    (feval-fn plfunc)
	    (f2eval-data plpointer)
	    (nx plint)
	    (ny plint)
	    (kx plint)
	    (lx plint)
	    (ky plint)
	    (ly plint)
	    (clevel *plflt nlevel)
	    (nlevel plint)
	    (pltr-fn plfunc)
	    (pltr-data plpointer))
	    
(pl-defcfun ("c_plcpstrm" plcpstrm) :void 
	    (iplsr plint)
	    (flags plint))

(pl-defcfun ("pldid2pc" pldid2pc) :void 
	(xmin *plflt :in-out)
	(ymin *plflt :in-out)
	(xmax *plflt :in-out)
	(ymax *plflt :in-out))

(pl-defcfun ("pldip2dc" pldip2dc) :void 
	(xmin *plflt :in-out)
	(ymin *plflt :in-out)
	(xmax *plflt :in-out)
	(ymax *plflt :in-out))

(pl-defcfun ("c_plend" plend) :void)

(pl-defcfun ("c_plend1" plend1) :void)

(pl-defcfun ("c_plenv" plenv) :void 
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt)
	    (just plint)
	    (axis plint))

(pl-defcfun ("c_plenv0" plenv0) :void 
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt)
	    (just plint)
	    (axis plint))

(pl-defcfun ("c_pleop" pleop) :void)

(pl-defcfun ("c_plerrx" plerrx) :void 
	    (n plint)
	    (xmin *plflt n)
	    (xmax *plflt n)
	    (y *plflt n))

(pl-defcfun ("c_plerry" plerry) :void 
	    (n plint)
	    (x *plflt n)
	    (ymin *plflt n)
	    (ymax *plflt n))

(pl-defcfun ("c_plfamadv" plfamadv) :void)

(defcallback plfill-fn :void ((n plint) (x *plflt) (y *plflt))
  (c-plfill n x y))

(pl-defcfun ("c_plfill" plfill) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n))

(pl-defcfun ("c_plfill3" plfill3) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n)
	    (z *plflt n))

(pl-defcfun ("c_plflush" plflush) :void)

(pl-defcfun ("c_plfont" plfont) :void 
	    (ifont plint))

(pl-defcfun ("c_plfontld" plfontld) :void 
	    (fnt plint))

(pl-defcfun ("c_plgchr" plgchr) :void 
	    (p_def *plflt 1)
	    (p_ht *plflt 1))

(pl-defcfun ("c_plgcol0" plgcol0) :void 
	    (icol0 plint)
	    (r *plint 1)
	    (g *plint 1)
	    (b *plint 1))

(pl-defcfun ("c_plgcol0a" plgcol0a) :void 
	    (icol0 plint)
	    (r *plint 1)
	    (g *plint 1)
	    (b *plint 1)
	    (a *plflt 1))

(pl-defcfun ("c_plgcolbg" plgcolbg) :void 
	    (r *plint 1)
	    (g *plint 1)
	    (b *plint 1))

(pl-defcfun ("c_plgcolbga" plgcolbga) :void 
	    (r *plint 1)
	    (g *plint 1)
	    (b *plint 1)
	    (a *plflt 1))

(pl-defcfun ("c_plgcompression" plgcompression) :void 
	    (compression *plint 1))

(pl-defcfun ("c_plgdev" plgdev) :void 
	(p_dev plstr 80))

(pl-defcfun ("c_plgdidev" plgdidev) :void 
	    (p_mar *plflt 1)
	    (p_aspect *plflt 1)
	    (p_jx *plflt 1)
	    (p_jy *plflt 1))

(pl-defcfun ("c_plgdiori" plgdiori) :void 
	    (p_rot *plflt 1))

(pl-defcfun ("c_plgdiplt" plgdiplt) :void 
	    (p_xmin *plflt 1)
	    (p_ymin *plflt 1)
	    (p_xmax *plflt 1)
	    (p_ymax *plflt 1))

(pl-defcfun ("c_plgfam" plgfam) :void 
	    (p_fam *plint 1)
	    (p_num *plint 1)
	    (p_bmax *plint 1))

(pl-defcfun ("c_plgfci" plgfci) :void 
	    (pfci *plunicode 1))

(pl-defcfun ("c_plgfnam" plgfnam) :void 
	    (fnam plstr 80))

(pl-defcfun ("c_plgfont" plgfont) :void 
	    (p_family *plint 1)
	    (p_style *plint 1)
	    (p_weight *plint 1))

(pl-defcfun ("c_plglevel" plglevel) :void 
	    (p_level *plint 1))

(pl-defcfun ("c_plgpage" plgpage) :void 
	    (p_xp *plflt 1)
	    (p_yp *plflt 1)
	    (p_xleng *plint 1)
	    (p_yleng *plint 1)
	    (p_xoff *plint 1)
	    (p_yoff *plint 1))

(pl-defcfun ("c_plgra" plgra) :void)

(pl-defcfun ("c_plgradient" plgradient) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n)
	    (angle plflt))

(defun plgriddata (x y z xg yg type data)
  (let ((sx (length x))
	(sy (length y))
	(f (foreign-alloc :pointer)))
    (plalloc2dgrid f sx sy)
    (let ((zg (mem-aref f :pointer)))
      (pl-plgriddata x y z xg yg zg type data)
      (let ((lisp-mat (make-array (list (length xg) (length yg)) :initial-element 0.0d0 :element-type 'double-float)))
	(dotimes (x (length xg))
	  (dotimes (y (length yg))
	    (setf (aref lisp-mat x y) (mem-aref (mem-aref zg :pointer x) :double y))))
	(plfree2dgrid zg sx sy)
	(foreign-free f)
	lisp-mat))))

(export 'plgriddata (package-name *package*))
    
(pl-defcfun ("c_plgriddata" pl-plgriddata) :void 
	    (x *plflt npts)
	    (y *plflt npts)
	    (z *plflt npts)
	    (npts plint)
	    (xg *plflt nptsx)
	    (nptsx plint)
	    (yg *plflt nptsy)
	    (nptsy plint)
	    (zg plpointer)
	    (type plint)
	    (data plflt))

(pl-defcfun ("c_plgspa" plgspa) :void 
	    (xmin *plflt 1)
	    (xmax *plflt 1)
	    (ymin *plflt 1)
	    (ymax *plflt 1))

(pl-defcfun ("c_plgstrm" plgstrm) :void 
	    (p_strm *plint 1))

(pl-defcfun ("c_plgver" plgver) :void 
	    (p_ver plstr 80))

(pl-defcfun ("c_plgvpd" plgvpd) :void 
	    (p_xmin *plflt 1)
	    (p_xmax *plflt 1)
	    (p_ymin *plflt 1)
	    (p_ymax *plflt 1))

(pl-defcfun ("c_plgvpw" plgvpw) :void 
	    (p_xmin *plflt 1)
	    (p_xmax *plflt 1)
	    (p_ymin *plflt 1)
	    (p_ymax *plflt 1))

(pl-defcfun ("c_plgxax" plgxax) :void 
	    (p_digmax *plint 1)
	    (p_digits *plint 1))

(pl-defcfun ("c_plgyax" plgyax) :void 
	    (p_digmax *plint 1)
	    (p_digits *plint 1))

(pl-defcfun ("c_plgzax" plgzax) :void 
	    (p_digmax *plint 1)
	    (p_digits *plint 1))

(pl-defcfun ("c_plhist" plhist) :void 
	    (n plint)
	    (data *plflt n)
	    (datmin plflt)
	    (datmax plflt)
	    (nbin plint)
	    (opt plint))

;; deprecated
(pl-defcfun ("c_plhls" plhls) :void 
	    (h plflt)
	    (l plflt)
	    (s plflt))

(pl-defcfun ("c_plhlsrgb" plhlsrgb) :void 
	    (h plflt)
	    (l plflt)
	    (s plflt)
	    (p_r *plflt 1)
	    (p_g *plflt 1)
	    (p_b *plflt 1))


(pl-defcfun ("c_plinit" plinit) :void)

(pl-defcfun ("c_pljoin" pljoin) :void 
	    (x1 plflt)
	    (y1 plflt)
	    (x2 plflt)
	    (y2 plflt))

(pl-defcfun ("c_pllab" pllab) :void 
	    (xlabel plstr)
	    (ylabel plstr)
	    (tlabel plstr))

(pl-defcfun ("c_pllightsource" pllightsource) :void 
	    (x plflt)
	    (y plflt)
	    (z plflt))

(pl-defcfun ("c_plline" plline) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n))

(pl-defcfun ("c_plline3" plline3) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n)
	    (z *plflt n))

(pl-defcfun ("c_pllsty" pllsty) :void 
	    (lin plint))

(callback-closure map-fn #'(lambda(n x y) (declare (ignore n x y)) 0) :void (n plint) (x *plflt) (y *plflt))

(pl-defcfun ("c_plmap" plmap) :void
	    (map-fn plfunc)
	    (type plstr)
	    (minlong plflt)
	    (maxlong plflt)
	    (minlat plflt)
	    (maxlat plflt))

(pl-defcfun ("c_plmeridians" plmeridians) :void
	    (map-fn plfunc)
	    (dlong plflt)
	    (dlat plflt)
	    (minlong plflt)
	    (maxlong plflt)
	    (minlat plflt)
	    (maxlat plflt))

(pl-defcfun ("c_plmesh" plmesh) :void 
	    (x *plflt nx)
	    (y *plflt ny)
	    (z **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (opt plint))

(pl-defcfun ("c_plmeshc" plmeshc) :void 
	    (x *plflt nx)
	    (y *plflt ny)
	    (z **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (opt plint)
	    (clevel *plflt nlevel)
	    (nlevel plint))

(pl-defcfun ("c_plmkstrm" plmkstrm) :void 
	    (p_strm *plint 1))

(pl-defcfun ("c_plmtex" plmtex) :void 
	    (side plstr)
	    (disp plflt)
	    (pos plflt)
	    (just plflt)
	    (text plstr))

(pl-defcfun ("c_plmtex3" plmtex3) :void
	    (side plstr)
	    (disp plflt)
	    (pos plflt)
	    (just plflt)
	    (text plstr))

(pl-defcfun ("c_plot3d" plot3d) :void 
	    (x *plflt nx)
	    (y *plflt ny)
	    (z **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (opt plint)
	    (side plint))

(pl-defcfun ("c_plot3dc" plot3dc) :void 
	    (x *plflt nx)
	    (y *plflt ny)
	    (z **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (opt plint)
	    (clevel *plflt nlevel)
	    (nlevel plint))

(pl-defcfun ("c_plot3dcl" plot3dcl) :void 
	    (x *plflt nx)
	    (y *plflt ny)
	    (z **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (opt plint)
	    (clevel *plflt nlevel)
	    (nlevel plint)
	    (ixstart plint)
	    (ixn plint)
	    (indexymin *plint ixn)
	    (indexymax *plint ixn))

(pl-defcfun ("c_plpat" plpat) :void 
	    (nlin plint)
	    (inc *plint nlin)
	    (del *plint nlin))

(pl-defcfun ("c_plpath" plpath) :void
	    (n plint)
	    (x1 plflt)
	    (y1 plflt)
	    (x2 plflt)
	    (y2 plflt))

(pl-defcfun ("c_plpoin" plpoin) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n)
	    (code plint))

(pl-defcfun ("c_plpoin3" plpoin3) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n)
	    (z *plflt n)
	    (code plint))

(pl-defcfun ("c_plpoly3" plpoly3) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n)
	    (z *plflt n)
	    (draw *plint n)
	    (ifcc plbool))

(pl-defcfun ("c_plprec" plprec) :void 
	    (setp plint)
	    (prec plint))

(pl-defcfun ("c_plpsty" plpsty) :void 
	    (patt plint))

(pl-defcfun ("c_plptex" plptex) :void 
	    (x plflt)
	    (y plflt)
	    (dx plflt)
	    (dy plflt)
	    (just plflt)
	    (text plstr))

(pl-defcfun ("c_plptex3" plptex3) :void 
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


(pl-defcfun ("c_plrandd" plrandd) :double)

(pl-defcfun ("c_plreplot" plreplot) :void)

;; deprecated
(pl-defcfun ("c_plrgb" plrgb) :void 
	    (r plflt)
	    (g plflt)
	    (b plflt))

;; deprecated
(pl-defcfun ("c_plrgb1" plrgb1) :void 
	    (r plint)
	    (g plint)
	    (b plint))

(pl-defcfun ("c_plrgbhls" plrgbhls) :void 
	    (r plflt)
	    (g plflt)
	    (b plflt)
	    (p_h *plflt 1)
	    (p_l *plflt 1)
	    (p_s *plflt 1))

(pl-defcfun ("c_plschr" plschr) :void 
	    (def plflt)
	    (scale plflt))

(pl-defcfun ("c_plscmap0" plscmap0) :void 
	    (r *plint ncol0)
	    (g *plint ncol0)
	    (b *plint ncol0)
	    (ncol0 plint))

(pl-defcfun ("c_plscmap0a" plscmap0a) :void 
	    (r *plint ncol0)
	    (g *plint ncol0)
	    (b *plint ncol0)
	    (a *plflt ncol0)
	    (ncol0 plint))

(pl-defcfun ("c_plscmap0n" plscmap0n) :void 
	    (ncol0 plint))

(pl-defcfun ("c_plscmap1" plscmap1) :void 
	    (r *plint ncol1)
	    (g *plint ncol1)
	    (b *plint ncol1)
	    (ncol1 plint))

(pl-defcfun ("c_plscmap1a" plscmap1a) :void 
	    (r *plint ncol1)
	    (g *plint ncol1)
	    (b *plint ncol1)
	    (a *plflt ncol1)
	    (ncol1 plint))

(pl-defcfun ("c_plscmap1l" plscmap1l) :void 
	    (itype plbool)
	    (npts plint)
	    (intensity *plflt npts)
	    (coord1 *plflt npts)
	    (coord2 *plflt npts)
	    (coord3 *plflt npts)
	    (rev *plbool n))

(pl-defcfun ("c_plscmap1la" plscmap1la) :void 
	    (itype plbool)
	    (npts plint)
	    (intensity *plflt npts)
	    (coord1 *plflt npts)
	    (coord2 *plflt npts)
	    (coord3 *plflt npts)
	    (coord4 *plflt npts)
	    (rev *plbool npts))

(pl-defcfun ("c_plscmap1n" plscmap1n) :void 
	    (ncol1 plint))

(pl-defcfun ("c_plscol0" plscol0) :void 
	    (icol0 plint)
	    (r plint)
	    (g plint)
	    (b plint))

(pl-defcfun ("c_plscol0a" plscol0a) :void 
	    (icol0 plint)
	    (r plint)
	    (g plint)
	    (b plint)
	    (a plflt))

(pl-defcfun ("c_plscolbg" plscolbg) :void 
	    (r plint)
	    (g plint)
	    (b plint))

(pl-defcfun ("c_plscolbga" plscolbga) :void 
	    (r plint)
	    (g plint)
	    (b plint)
	    (a plflt))

(pl-defcfun ("c_plscolor" plscolor) :void 
	    (color plint))

(pl-defcfun ("c_plscompression" plscompression) :void 
	    (compression plint))

(pl-defcfun ("c_plsdev" plsdev) :void 
	    (dev plstr))

(pl-defcfun ("c_plsdidev" plsdidev) :void 
	    (mar plflt)
	    (aspect plflt)
	    (jx plflt)
	    (jy plflt))

(pl-defcfun ("c_plsdimap" plsdimap) :void 
	    (dimxmin plint)
	    (dimxmax plint)
	    (dimymin plint)
	    (dimymax plint)
	    (dimxpmm plflt)
	    (dimypmm plflt))

(pl-defcfun ("c_plsdiori" plsdiori) :void 
	    (rot plflt))

(pl-defcfun ("c_plsdiplt" plsdiplt) :void 
	    (xmin plflt)
	    (ymin plflt)
	    (xmax plflt)
	    (ymax plflt))

(pl-defcfun ("c_plsdiplz" plsdiplz) :void 
	    (xmin plflt)
	    (ymin plflt)
	    (xmax plflt)
	    (ymax plflt))

(pl-defcfun ("c_plseed" plseed) :void 
	    (seed plint))

(pl-defcfun ("c_plsesc" plsesc) :void 
	    (esc plchar))

(pl-defcfun ("c_plsfam" plsfam) :void 
	    (fam plint)
	    (num plint)
	    (bmax plint))

(pl-defcfun ("c_plsfci" plsfci) :void 
	    (fci plunicode))

;; This is inside a closure so that the file name & its associated storage
;; is preserved until the next time this function is called

(let ((c-fname nil))
  (defun plsfnam (fname)
    (when c-fname
      (foreign-string-free c-fname))
    (setf c-fname (foreign-string-alloc fname))
    (c-plsfnam c-fname)))

(export 'plsfnam (package-name *package*))

(defcfun ("c_plsfnam" c-plsfnam) :void 
  (fnam plstr))

(pl-defcfun ("c_plsfont" plsfont) :void 
	    (family plint)
	    (style plint)
	    (weight plint))

;; Note that for these functions you have no choice when it comes to the fill function,
;; as this is what the plplot docs suggest. You can set a user defined exclude function 
;; and a user defined contour function by setting my-defined-fn or my-pltr-fn inside
;; their respective closures.

;; use the no exclusion function or set your own with this closure

(callback-closure defined-fn #'(lambda(x y) (declare (ignore x y)) 1) plint (x plflt) (y plflt))

(defun plshade (a left right bottom top shade-min shade-max sh-cmap sh-color sh-width min-color min-width max-color max-width rectangular &optional (gridx nil) (gridy nil))
  (with-plcgrid (plc-grid gridx gridy (array-dimension a 0) (array-dimension a 1))
    (pl-plshade a left right bottom top shade-min shade-max sh-cmap sh-color sh-width min-color min-width max-color max-width rectangular plc-grid)))

(export 'plshade (package-name *package*))

(pl-defcfun ("c_plshade" pl-plshade) :void
	    (a **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (defined-fn plfunc)
	    (left plflt)
	    (right plflt)
	    (bottom plflt)
	    (top plflt)
	    (shade-min plflt)
	    (shade-max plflt)
	    (sh-cmap plint)
	    (sh-color plflt)
	    (sh-width plint)
	    (min-color plint)
	    (min-width plint)
	    (max-color plint)
	    (max-width plint)
	    (plfill-fn plfunc)
	    (rectangular plbool)
	    (pltr-fn plfunc)
	    (pltr-data plpointer))

(defun plshades (a xmin xmax ymin ymax clevel fill-width cont-color cont-width rectangular &optional (gridx nil) (gridy nil))
  (with-plcgrid (plc-grid gridx gridy (array-dimension a 0) (array-dimension a 1))
    (pl-plshades a xmin xmax ymin ymax clevel fill-width cont-color cont-width rectangular plc-grid)))

(export 'plshades (package-name *package*))
    
(pl-defcfun ("c_plshades" pl-plshades) :void
	    (a **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (defined-fn plfunc)
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt)
	    (clevel *plflt nlevel)
	    (nlevel plint)
	    (fill-width plint)
	    (cont-color plint)
	    (cont-width plint)
	    (plfill-fn plfunc)
	    (rectangular plbool)
	    (pltr-fn plfunc)
	    (pltr-data plpointer))


;; The "friendly" form of this one was deemed to be adequately covered by the above 
;; functions, so only the "cffi" version of the function is provided.

(defcfun ("c_plshade1" c-plshade1) :void
  (a **plflt) ; According the plplot-5.5.3.pdf this should be an array [nx][ny], but in plplot.h its type is plflt *...
  (nx plint)
  (ny plint)
  (defined-fn plfunc)
  (left plflt)
  (right plflt)
  (bottom plflt)
  (top plflt)
  (shade-min plflt)
  (shade-max plflt)
  (sh-cmap plint)
  (sh-color plflt)
  (sh-width plint)
  (min-color plint)
  (min-width plint)
  (max-color plint)
  (max-width plint)
  (plfill-fn plfunc)
  (rectangular plbool)
  (pltr-fn plfunc)
  (pltr-data plpointer))

(export 'plshade1 (package-name *package*))

;; Left this one out for the time being since its documentation is a bit thin.
;; I believe that it would let you make shade plots of functions.
	    
;(defcfun ("plfshade" plfshade) :void 
;	PLFLT (*f2eval) (PLINT, PLINT, PLPointer), 	 
; PLPointer f2eval_data, 	 PLFLT (*c2eval) (PLINT, PLINT, PLPointer), 	 
; PLPointer c2eval_data, 	 PLINT nx, PLINT ny, 	 
; PLFLT left, PLFLT right, PLFLT bottom, PLFLT top, 	 PLFLT shade_min, PLFLT shade_max, 	 
; PLINT sh_cmap, PLFLT sh_color, PLINT sh_width, 	 PLINT min_color, PLINT min_width, 	 
; PLINT max_color, PLINT max_width, 	 void (*fill) (PLINT, PLFLT *, PLFLT *), PLBOOL rectangular, 	 
; void (*pltr) (PLFLT, PLFLT, PLFLT *, PLFLT *, PLPointer), 	 PLPointer pltr_data

;(pl-defcfun ("c_plsfam" plsfam) :void 

(pl-defcfun ("c_plslabelfunc" plslabelfunc) :void 
	    (label-func plfunc)
	    (label-data plpointer))

;;
;; plsmem is broken into three parts:
;;
;; plsmem-alloc : allocates the memory to be used for plotting
;; plsmem-get : gets a lisp friendly version of the bitmap plot
;; plsmem-free : frees the memory associated with the plot
;;
;; These are all inside a closure for user convenience (hopefully...)
;;

(let ((xsize nil)
      (ysize nil)
      (mem nil))

  (defun plsmem-alloc (maxx maxy)
    "allocates memory to be used for in memory plotting"
    (when mem
      (foreign-free mem))
    (setf mem (foreign-alloc :unsigned-char :count (* 3 maxx maxy)))
    (pl-plsmem maxx maxy mem)
    (setf xsize maxx)
    (setf ysize maxy))

  (defun plsmem-get ()
    "gets a lisp array version of the bitmap plot"
    (let ((image (make-array (list xsize ysize 3) :element-type 'unsigned-byte)))
      (dotimes (x xsize)
	(dotimes (y ysize)
	  (dotimes (rgb 3)
	    (setf (aref image x y rgb) (mem-aref mem :unsigned-char (+ (* 3 xsize y) (* 3 x) rgb))))))
      image))

  (defun plsmem-free ()
    "free memory that was used for in memory plotting"
    (when mem
      (foreign-free mem)
      (setf mem nil)))

  (export 'plsmem-alloc (package-name *package*))
  (export 'plsmem-get (package-name *package*))
  (export 'plsmem-free (package-name *package*)))

(pl-defcfun ("c_plsmem" pl-plsmem) :void 
	    (maxx plint)
	    (maxy plint)
	    (plotmem plpointer))

(pl-defcfun ("c_plsmin" plsmin) :void 
	    (def plflt)
	    (scale plflt))

(pl-defcfun ("c_plsori" plsori) :void 
	    (ori plint))

(pl-defcfun ("c_plspage" plspage) :void 
	    (xp plflt)
	    (yp plflt)
	    (xleng plint)
	    (yleng plint)
	    (xoff plint)
	    (yoff plint))

(pl-defcfun ("c_plspause" plspause) :void 
	    (pause plbool))

(pl-defcfun ("c_plsstrm" plsstrm) :void 
	    (strm plint))

(pl-defcfun ("c_plssub" plssub) :void 
	    (nx plint)
	    (ny plint))

(pl-defcfun ("c_plssym" plssym) :void 
	    (def plflt)
	    (scale plflt))

(pl-defcfun ("c_plstar" plstar) :void 
	    (nx plint)
	    (ny plint))

(pl-defcfun ("c_plstart" plstart) :void 
	    (devname plstr)
	    (nx plint)
	    (ny plint))

(pl-defcfun ("c_plstransform" plstransform) :void 
	    (transform-fun plfunc)
	    (data plpointer))

(pl-defcfun ("c_plstripa" plstripa) :void 
	    (id plint)
	    (pen plint)
	    (x plflt)
	    (y plflt))


;; Expects colline, styline & legline to 4 element lists containing the
;; appropriate color, style and name for each pen

(defun plstripc (xspec yspec xmin xmax xjump ymin ymax xlpos ylpos y_ascl acc colbox collab colline styline legline labx laby labtop)
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
	    (colline plpointer)
	    (styline plpointer)
	    (legline plpointer)
	    (labx plstr)
	    (laby plstr)
	    (labtop plstr))

(pl-defcfun ("c_plstripd" plstripd) :void 
	    (id plint))

(pl-defcfun ("c_plimagefr" plimagefr) :void 
	    (idata **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt)
	    (zmin plflt)
	    (zmax plflt)
	    (valuemin plflt)
	    (valuemax plflt)
	    (pltr-fn plfunc)
	    (pltr-data plpointer))

(pl-defcfun ("plimage" plimage) :void 
	    (data **plflt (nx ny))
	    (nx plint)
	    (ny plint)
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

(pl-defcfun ("c_plstyl" plstyl) :void 
	    (nms plint)
	    (mark *plint n)
	    (space *plint n))

(pl-defcfun ("c_plsurf3d" plsurf3d) :void 
	    (x *plflt nx)
	    (y *plflt ny)
	    (z **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (opt plint)
	    (clevel *plflt nlevel)
	    (nlevel plint))

;; See x08l.lisp for an example of how to use this function.
(pl-defcfun ("plfsurf3d" plfsurf3d) :void
	    (x *plflt nx)
	    (y *plflt ny)
	    (zops plpointer)
	    (zp plpointer)
	    (nx plint)
	    (ny plint)
	    (opt plint)
	    (clevel *plflt nlevel)
	    (nlevel plint))

;; These are helper functions for plf... functions, they
;; return a pointer to a "zops" function.
(defcfun "plf2ops_c" :pointer)
(defcfun "plf2ops_grid_c" :pointer)
(defcfun "plf2ops_grid_row_major" :pointer)
(defcfun "plf2ops_grid_col_major" :pointer)

(export 'plf2ops-c (package-name *package*))
(export 'plf2ops-grid-c (package-name *package*))
(export 'plf2ops-grid-row-major (package-name *package*))
(export 'plf2ops-grid-col-major (package-name *package*))

(pl-defcfun ("c_plsurf3dl" plsurf3dl) :void 
	    (x *plflt nx)
	    (y *plflt ny)
	    (z **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (opt plint)
	    (clevel *plflt nlevel)
	    (nlevel plint)
	    (ixstart plint)
	    (ixn plint)
	    (indexymin *plint ixn)
	    (indexymax *plint ixn))

(pl-defcfun ("c_plsvpa" plsvpa) :void 
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt))

(pl-defcfun ("c_plsxax" plsxax) :void 
	    (digmax plint)
	    (digits plint))

(pl-defcfun ("plsxwin" plsxwin) :void 
	    (window_id plint))

(pl-defcfun ("c_plsyax" plsyax) :void 
	    (digmax plint)
	    (digits plint))

(pl-defcfun ("c_plsym" plsym) :void 
	    (n plint)
	    (x *plflt n)
	    (y *plflt n)
	    (code plint))

(pl-defcfun ("c_plszax" plszax) :void 
	    (digmax plint)
	    (digits plint))

(pl-defcfun ("c_pltext" pltext) :void)

(pl-defcfun ("c_pltimefmt" pltimefmt) :void
	    (fmt plstr))
			   
(pl-defcfun ("c_plvasp" plvasp) :void 
	    (aspect plflt))

(pl-defcfun ("c_plvpas" plvpas) :void 
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt)
	    (aspect plflt))

(pl-defcfun ("c_plvpor" plvpor) :void 
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt))

(pl-defcfun ("c_plvsta" plvsta) :void)

(pl-defcfun ("c_plw3d" plw3d) :void 
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

(pl-defcfun ("c_plwid" plwid) :void 
	    (width plint))

(pl-defcfun ("c_plwind" plwind) :void 
	    (xmin plflt)
	    (xmax plflt)
	    (ymin plflt)
	    (ymax plflt))

(pl-defcfun ("c_plxormod" plxormod) :void 
	    (mode plbool)
	    (status plbool 1))


;; Why do plgdevs & plgfiledevs take a char*** when they expect you to allocate enough 
;; space for a bunch of pointers to strings anyway? Why not char**?

(defun get-strings (c-ptr n)
  (let ((c-strings (mem-aref c-ptr :pointer))
	(strings nil))
    (dotimes (i n)
      (push (foreign-string-to-lisp (mem-aref c-strings :pointer i)) strings))
    (reverse strings)))

(defun create-char*** ()
  (let ((c-ptr (foreign-alloc :pointer)))
    (setf (mem-aref c-ptr :pointer) (foreign-alloc :pointer :count 50))
    c-ptr))

(defun free-char*** (c-ptr)
  (foreign-free (mem-aref c-ptr :pointer))
  (foreign-free c-ptr))

(defun alist (lst1 lst2)
  (let ((ret nil))
    (dotimes (i (length lst1))
      (push (list (elt lst1 i) (elt lst2 i)) ret))
    (reverse ret)))

(defun generic-devs (devfn)
  (let* ((c-menu (create-char***))
	 (c-dev (create-char***))
	 (ndev (funcall devfn c-menu c-dev))
	 (al (alist (get-strings c-menu ndev) (get-strings c-dev ndev))))
    (free-char*** c-menu)
    (free-char*** c-dev)
    al))

(defun plgfiledevs ()
  (generic-devs #'pl-plgfiledevs))

(export 'plgfiledevs (package-name *package*))

(pl-defcfun ("plgFileDevs" pl-plgfiledevs) :void 
	    (p_menustr plpointer)
	    (p_devname plpointer)
	    (p_ndev *plint 1))

(defun plgdevs ()
  (generic-devs #'pl-plgdevs))

(export 'plgdevs (package-name *package*))

(pl-defcfun ("plgDevs" pl-plgdevs) :void 
	    (p_menustr plpointer)
	    (p_devname plpointer)
	    (p_ndev *plint 1))


;; These would enable callbacks to user defined functions at various key points in
;; the graphing process. We use plsexit to set our own error handler that will
;; throw a Lisp side error and keep PLplot from calling exit().

;(defcfun ("plsKeyEH" plskeyeh) :void 
;	void (*KeyEH) (PLGraphicsIn *, void *, int *), void *KeyEH_data
;
;(defcfun ("plsButtonEH" plsbuttoneh) :void 
;	void (*ButtonEH) (PLGraphicsIn *, void *, int *), 	    void *ButtonEH_data
;
;(defcfun ("plsbopH" plsboph) :void 
;	void (*handler) (void *, int *), void *handler_data
;
;(defcfun ("plseopH" plseoph) :void 
;	void (*handler) (void *, int *), void *handler_data

(defcallback trap-plsexit :int ((message plstr))
  (error "PLplot error encountered (~A). The current plotting stream is likely corrupted."
	 (foreign-string-to-lisp message)))

(defcfun ("plsexit" plsexit) :void
  (plsexit-fn plfunc))

(plsexit (callback trap-plsexit))

;(defcfun ("plsabort" plsabort) :void 
;	void (*handler) (char *)

;; "Set the variables to be used for storing error info"...
;; FIXME: What does this function do?

;(defcfun ("plsError" plserror) :void 
;	(*errcode plint)
;	(*errmsg char))


;; These functions are designed to be passed as arguments to other plplot functions like 
;; the shade series of functions. pltr1 & pltr2 let you interpolate your data onto other
;; non-rectangular grids, but you have to pass them the appropriate information via the
;; pltr_data pointer or they will exit & take down lisp in the process. I've tried to make
;; this a little easier for those functions that might take these as arguments by taking
;; care of the formatting of the pltr_data structure.

(defcfun ("pltr0" pltr0) :void 
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr_data plpointer))

(export 'pltr0 (package-name *package*))

(defcfun ("pltr1" pltr1) :void 
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr_data plpointer))

(export 'pltr1 (package-name *package*))

(defcfun ("pltr2" pltr2) :void 
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr_data plpointer))

(export 'pltr2 (package-name *package*))

;; set which one of these function you want to use, or pass in your own with this closure

(callback-closure pltr-fn #'pltr0 :void (x plflt) (y plflt) (tx *plflt) (ty *plflt) (pltr-data plpointer))

;; This one is not supported, if you want to use it then you have to properly handle the formatting of pltr_data.

(defcfun ("pltr2p" pltr2p) :void 
  (x plflt)
  (y plflt)
  (tx *plflt)
  (ty *plflt)
  (pltr_data plpointer))

(export 'pltr2p (package-name *package*))


;; These are not exported
;
;(defcfun ("pltr0f" pltr0f) :void 
;  (x plflt)
;  (y plflt)
;  (tx *plflt)
;  (ty *plflt)
;  (pltr_data plpointer))
;
;(defcfun ("pltr2f" pltr2f) :void
;  (x plflt)
;  (y plflt)
;  (tx *plflt)
;  (ty *plflt)
;  (pltr_data pointer))


;; No mention of this function anywhere in the plplot docs, but it was in plplot.h.
;; Doesn't seem to be exported however.

;(pl-defcfun ("xform" xform) :void 
;	    (x plflt)
;	    (y plflt)
;	    (tx *plflt 1)
;	    (ty *plflt 1))

;; These are functions are used by plcont (&others?) to evaluate data on an arbitrary 2D grid

(pl-defcfun ("plf2eval2" plf2eval2) plflt
	    (ix plint)
	    (iy plint)
	    (plf2eval_data plpointer))  ; **obj, as created by make-matrix

(pl-defcfun ("plf2eval" plf2eval) plflt
	    (ix plint)
	    (iy plint)
	    (plf2eval_data plpointer))  ; *obj as created by foreign-alloc (C ordering)

(pl-defcfun ("plf2evalr" plf2evalr) plflt
	    (ix plint)
	    (iy plint)
	    (plf2eval_data plpointer)) ; *obj as created by foreign-alloc (Fortran ordering)

(callback-closure feval-fn #'plf2eval2 plflt (ix plint) (iy plint) (plf-data plpointer))

;; FIXME: These were left out since it wasn't clear to me how to use them.
;
;(pl-defcfun ("plClearOpts" plclearopts) :void)
;
;(pl-defcfun ("plResetOpts" plresetopts) :void)
;
;(defcfun ("plMergeOpts" plmergeopts) :int 
;	(*options ploptiontable)
;	(*name char)
;	(**notes char))

(pl-defcfun ("plSetUsage" plsetusage) :void 
	    (program_string plstr)
	    (usage_string plstr))

(pl-defcfun ("c_plsetopt" plsetopt) :int 
	    (opt plstr)
	    (optarg plstr))

;not supposed to use this one?
;(pl-defcfun ("plSetOpt" plsetopt) :int 
;	(opt plstr)
;	(optarg plstr))


;; Options should be a list containing strings of command line options. This
;; works but is commented out due the dangers associated with using it. Uncomment
;; it if you like, but heed the warning... Otherwise plsetopt is a better choice
;; since it won't kill your lisp process if it gets an unrecognized option.
;;
;; Warning: If you get them wrong your lisp session will almost surely meet
;; an untimely end.
;
;(defun plparseopts (options mode)
;  (push "foo" options)  ; need a dummy zero argument...
;  (let* ((nopts (length options))
;	 (p_argc (foreign-alloc :int :initial-element nopts))
;	 (argv (foreign-alloc :pointer :count nopts)))
;    (dotimes (i nopts)
;      (setf (mem-aref argv :pointer i) (foreign-string-alloc (elt options i))))
;    (pl-plparseopts p_argc argv mode)
;    (foreign-free p_argc)
;    (dotimes (i nopts)
;      (foreign-string-free (mem-aref argv :pointer i)))
;    (foreign-free argv)))
;
;(export 'plparseopts (package-name *package*))
;
;(pl-defcfun ("c_plparseopts" pl-plparseopts) :int 
;	    (p_argc plpointer)
;	    (argv plpointer)
;	    (mode plint))

(pl-defcfun ("plOptUsage" ploptusage) :void)


;; Returns a file pointer that might be useful for passing to another C sub-routine
;; that wants to add something to the current plplot output file.

(defun plgfile ()
  (let ((fp (foreign-alloc :pointer)))
    (unwind-protect
	 (progn
	   (pl-plgfile fp)
	   (mem-aref fp :pointer))
      (foreign-free fp))))

(export 'plgfile (package-name *package*))

(defcfun ("plgfile" pl-plgfile) :void 
  (p_file plpointer))

;; Opens a file, typically used with file output plotting drivers.
;; FIXME: what is the right file mode?? Does it matter?

(defun plsfile (fname)
  (let ((name (foreign-string-alloc fname))
	(mode (foreign-string-alloc "wb")))
    (unwind-protect
	 (pl-plsfile (fopen name mode))
      (progn
	(foreign-string-free name)
	(foreign-string-free mode)))))

(export 'plsfile (package-name *package*))

(defcfun ("fopen" fopen) :pointer
  (name plstr)
  (mode plstr))

(defcfun ("plsfile" pl-plsfile) :void 
  (file plpointer))

(pl-defcfun ("plgesc" plgesc) :void 
	    (p_esc *plchar 1))

(pl-defcfun ("pl_cmd" pl-cmd) :void
	    (op plint)
	    (ptr plpointer))

;(defcfun ("pl_cmd" pl-cmd) :void 
;	PLINT op, void *ptr
;


;; These seem to be used for finding system commands & executables.
;; Currently not supported since there are probably better ways.
;
;(pl-defcfun ("plFindName" plfindname) :int 
;	    (p plstr 80))
;
;(defcfun ("plFindCommand" plfindcommand) :char * 
;	(*fn char))
;
;(defcfun ("plGetName" plgetname) :void 
;	(*dir char)
;	(*subdir char)
;	(*filename char)
;	(**filespec char))

(pl-defcfun ("plGetInt" plgetint) plint
	(s plstr))

(pl-defcfun ("plGetFlt" plgetflt) plflt
	(s plstr))

(pl-defcfun ("plAlloc2dGrid" plalloc2dgrid) :void 
	(f plpointer)
	(nx plint)
	(ny plint))

(pl-defcfun ("plFree2dGrid" plfree2dgrid) :void 
	(f plpointer)
	(nx plint)
	(ny plint))

(pl-defcfun ("plMinMax2dGrid" plminmax2dgrid) :void 
	    (f **plflt (nx ny))
	    (nx plint)
	    (ny plint)
	    (fmax *plflt 1)
	    (fmin *plflt 1))


;; Mouse events, this structure has a lot of members... And a 16 byte string right
;; in the middle to add to the fun... 

;(defparameter plg-offset (+ 16 (* (foreign-type-size :int))))

(defmacro init-plgraphicsin ()
    `(defcstruct plgraphicsin
       (type plint)         ; 2 bytes
       (start plint)        ; 2 bytes
       (keysym plint)       ; 2 bytes
       (button plint)       ; 2 bytes
       (subwindow plint)    ; 2 bytes
       (string :pointer)   ; 16 bytes
       (pX plint :offset ,(+ 16 (* 5 (foreign-type-size 'plint))))
       (pY plint)
       (dX plflt)
       (dY plflt)
       (wX plflt)
       (wY plflt)))

(init-plgraphicsin)

(defun init-slots (pin)
  (with-foreign-slots ((type start keysym button subwindow pX pY dX dY wX wY) pin plgraphicsin)
    (setf type -1)
    (setf start -1)
    (setf keysym -1)
    (setf button -1)
    (setf subwindow -1)
    (setf pX -1)
    (setf pY -1)
    (setf dX -1.0d0)
    (setf dY -1.0d0)
    (setf wX -1.0d0)
    (setf wY -1.0d0)))


;; This works in that it seems to return the correct X & Y coordinates of the mouse click (in pixels).
;; I'm not so sure about subwindow, but maybe I don't get that parameter.

(defun plgetcursor ()
  (with-foreign-object (pin 'plgraphicsin)
    (init-slots pin)
    (pl-plgetcursor pin)
    (with-foreign-slots ((start keysym button subwindow pX pY dX dY wX wY) pin plgraphicsin)
      (list start keysym button subwindow pX pY dX dY wX wY))))

;      (list start keysym button subwindow (foreign-string-to-lisp (foreign-slot-value pin 'plgraphicsin 'string)) pX pY dX dY wX wY))))

(export 'plgetcursor (package-name *package*))

(defcfun ("plGetCursor" pl-plgetcursor) plint
  (gin :pointer))

(defun pltranslatecursor (dX dY)
  (with-foreign-object (pin 'plgraphicsin)  
    (init-slots pin)  
    (setf (foreign-slot-value pin 'plgraphicsin 'dX) (coerce dX 'double-float))
    (setf (foreign-slot-value pin 'plgraphicsin 'dY) (coerce dY 'double-float))
    (pl-pltranslatecursor pin)
    (with-foreign-slots ((wX wY) pin plgraphicsin)
      (list wX wY))))

(export 'pltranslatecursor (package-name *package*))

(defcfun ("plTranslateCursor" pl-pltranslatecursor) plint
  (gin :pointer))


;; The following functions are in plplot.h but are deprecated. 
;; Presumably they will dissappear someday.
;
;(defcfun ("plParseOpts" plparseopts) :int 
;	(*p_argc int)
;	(**argv char)
;	(mode plint))

(pl-defcfun ("plHLS_RGB" plhls-rgb) :void 
	    (h plflt)
	    (l plflt)
	    (s plflt)
	    (p_r *plflt 1)
	    (p_g *plflt 1)
	    (p_b *plflt 1))

(pl-defcfun ("plRGB_HLS" plrgb-hls) :void 
	    (r plflt)
	    (g plflt)
	    (b plflt)
	    (p_h *plflt 1)
	    (p_l *plflt 1)
	    (p_s *plflt 1))


;;;;
;;;; Copyright (c) 2006 Hazen P. Babcock
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
