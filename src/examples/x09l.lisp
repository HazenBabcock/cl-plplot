;;;;
;;;; PLplot example 9
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(let ((xspa)
      (yspa))
  (defun x9-pltr (x y)
    (values (+ (* x xspa) (* y 0.0) -1.0)
	    (+ (* x 0.0) (* y yspa) -1.0)))
  (defun x9-set-xspa (new-xspa)
    (setf xspa new-xspa))
  (defun x9-set-yspa (new-yspa)
    (setf yspa new-yspa)))

(cffi:defcallback x9-pltr-callback :void ((x plflt) (y plflt) (tx *plflt) (ty *plflt) (pltr-data :pointer))
  (declare (ignore pltr-data))
  (multiple-value-bind (mx my) (x9-pltr x y)
    (setf (cffi:mem-aref tx 'plflt) (cffi:convert-to-foreign mx 'plflt)
	  (cffi:mem-aref ty 'plflt) (cffi:convert-to-foreign my 'plflt))))


(defun example9 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (let ((xpts 35)
	(ypts 46)
	(clevel (vector -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0)))
    (x9-set-xspa (/ 2.0 (- xpts 1.0)))
    (x9-set-yspa (/ 2.0 (- ypts 1.0)))
    (labels (
	     ; polar plot
	     (polar ()
	       (plenv -1.0 1.0 -1.0 1.0 0 -2)
	       (plcol0 1)
	       (let* ((perimeterpts 100)
		      (px (make-float-array perimeterpts))
		      (py (make-float-array perimeterpts)))
		 (dotimes (i perimeterpts)
		   (let ((theta (* (/ (* 2.0 3.14159) (- perimeterpts 1.0)) i)))
		     (setf (aref px i) (cos theta))
		     (setf (aref py i) (sin theta))))
		 (plline px py))
	       (let* ((rpts 40)
		      (thetapts 40)
		      (gridx (make-float-array (list rpts thetapts)))
		      (gridy (make-float-array (list rpts thetapts)))
		      (z (make-float-array (list rpts thetapts)))
		      (lev (make-float-array (list 10))))
		 (dotimes (i rpts)
		   (let ((r (/ i (- rpts 1.0))))
		     (dotimes (j thetapts)
		       (let ((theta (* (/ (* 2.0 pi) (- thetapts 1.0)) j)))
			 (setf (aref gridx i j) (* r (cos theta))
			       (aref gridy i j) (* r (sin theta))
			       (aref z i j) r)))))
		 (dotimes (i 10)
		   (setf (aref lev i) (+ 0.05 (* 0.10 i))))
		 (plcol0 2)
		 (with-pltr-data (pltr-data gridx gridy :pltr-fn 'pltr2-callback :z-vals z)
		   (plcont z 1 rpts 1 thetapts lev 'pltr2-callback pltr-data))
		 (plcol0 1)
		 (pllab "" "" "Polar Contour Plot")))

	     ; potential plot
	     (potential ()
	       (let* ((perimeterpts 100)
		      (rpts 40)
		      (thetapts 64)
		      (pnlevel 20)
		      (gridx (make-float-array (list rpts thetapts)))
		      (gridy (make-float-array (list rpts thetapts)))
		      (z (make-float-array (list rpts thetapts)))
		      (clevelneg (make-float-array pnlevel))
		      (clevelpos (make-float-array pnlevel)))
		 (dotimes (i rpts)
		   (let ((r (+ 0.5 i)))
		     (dotimes (j thetapts)
		       (let ((theta (* (/ (* 2.0 pi) (- thetapts 1.0)) (+ 0.5 j))))
			 (setf (aref gridx i j) (* r (cos theta))
			       (aref gridy i j) (* r (sin theta)))))))
		 (multiple-value-bind (xmin xmax) (min-max gridx)
		   (multiple-value-bind (ymin ymax) (min-max gridy)
		     (let* ((rmax (+ 0.5 (- rpts 1.0)))
			    (x0 (/ (+ xmin xmax) 2.0))
			    (y0 (/ (+ ymin ymax) 2.0))
			    (peps 0.05)
			    (xpmin (- xmin (* (abs xmin) peps)))
			    (xpmax (+ xmax (* (abs xmax) peps)))
			    (ypmin (- ymin (* (abs ymin) peps)))
			    (ypmax (+ ymax (* (abs ymax) peps)))
			    (eps 2.0)
			    (q1 1.0)
			    (d1 (/ rmax 4.0))
			    (q1i (/ (* (- q1) rmax) d1))
			    (d1i (/ (expt rmax 2.0) d1))
			    (q2 -1.0)
			    (d2 (/ rmax 4.0))
			    (q2i (/ (* (- q2) rmax) d2))
			    (d2i (/ (expt rmax 2.0) d2)))
		       (dotimes (i rpts)
			 (dotimes (j thetapts)
			   (let ((div1 (sqrt (+ (expt (- (aref gridx i j) d1) 2)
						(expt (- (aref gridy i j) d1) 2)
						(expt eps 2.0))))
				 (div1i (sqrt (+ (expt (- (aref gridx i j) d1i) 2)
						 (expt (- (aref gridy i j) d1i) 2)
						 (expt eps 2.0))))
				 (div2 (sqrt (+ (expt (- (aref gridx i j) d2) 2)
						(expt (+ (aref gridy i j) d2) 2)
						(expt eps 2.0))))
				 (div2i (sqrt (+ (expt (- (aref gridx i j) d2i) 2)
						 (expt (+ (aref gridy i j) d2i) 2)
						 (expt eps 2.0)))))
			     (setf (aref z i j) (+ (/ q1 div1)
						   (/ q1i div1i)
						   (/ q2 div2)
						   (/ q2i div2i))))))
		       (multiple-value-bind (zmin zmax) (min-max z)
			 (let ((dz (/ (- zmax zmin) pnlevel))
			       (nlevelneg 0)
			       (nlevelpos 0))
			   (dotimes (i pnlevel)
			     (let ((clevel (+ zmin (* (+ i 0.5) dz))))
			       (if (<= clevel 0)
				   (progn
				     (setf (aref clevelneg nlevelneg) clevel)
				     (incf nlevelneg))
				   (progn
				     (setf (aref clevelpos nlevelpos) clevel)
				     (incf nlevelpos)))))
			   (setf clevelneg (adjust-array clevelneg nlevelneg))
			   (setf clevelpos (adjust-array clevelpos nlevelpos))
			   (pladv 0)
			   (plcol0 1)
			   (plvpas 0.1 0.9 0.1 0.9 1.0)
			   (plwind xpmin xpmax ypmin ypmax)
			   (plbox "" 0.0 0 "" 0.0 0)
			   (plcol0 11)
			   (when (> nlevelneg 0)
			     (pllsty 2)
			     (with-pltr-data (pltr-data gridx gridy :pltr-fn 'pltr2-callback :z-vals z)
			       (plcont z 1 rpts 1 thetapts clevelneg 'pltr2-callback pltr-data)))
			   (when (> nlevelpos 0)
			     (pllsty 1)
			     (with-pltr-data (pltr-data gridx gridy :pltr-fn 'pltr2-callback)
			       (plcont z 1 rpts 1 thetapts clevelpos 'pltr2-callback pltr-data)))))
		       (let ((px (make-float-array perimeterpts))
			     (py (make-float-array perimeterpts)))
			 (dotimes (i perimeterpts)
			   (let ((theta (* (/ (* 2.0 3.14159) (- perimeterpts 1)) i)))
			     (setf (aref px i) (+ x0 (* rmax (cos theta)))
				   (aref py i) (+ y0 (* rmax (sin theta))))))
			 (plcol0 1)
			 (plline px py)
			 (plcol0 2)
			 (pllab "" "" "Shielded potential of charges in a conducting sphere"))))))))

      ;; main
      (let ((mark (make-int-array 1 1500))
	    (space (make-int-array 1 1500))
	    (z (make-float-array (list xpts ypts)))
	    (w (make-float-array (list xpts ypts)))
	    (xg1 (make-float-array xpts))
	    (yg1 (make-float-array ypts))
	    (gridx (make-float-array (list xpts ypts)))
	    (gridy (make-float-array (list xpts ypts))))
	(dotimes (i xpts)
	  (let ((xx (/ (- i (floor (/ xpts 2))) (floor (/ xpts 2)))))
	    (dotimes (j ypts)
	      (let ((yy (- (/ (- j (/ ypts 2)) (/ ypts 2)) 1.0)))
		(setf (aref z i j) (- (* xx xx) (* yy yy))
		      (aref w i j) (* 2.0 xx yy))))))
	(dotimes (i xpts)
	  (dotimes (j ypts)
	    (multiple-value-bind (xx yy) (x9-pltr i j)
	      (let ((argx (/ (* xx pi) 2.0))
		    (argy (/ (* yy pi) 2.0))
		    (distort 0.4))
		(setf (aref xg1 i) (+ xx (* distort (cos argx)))
		      (aref yg1 j) (- yy (* distort (cos argy)))
		      (aref gridx i j) (+ xx (* distort (cos argx) (cos argy)))
		      (aref gridy i j) (- yy (* distort (cos argx) (cos argy))))))))

	;; plot1
	(pl-setcontlabelformat 4 3)
	(pl-setcontlabelparam 0.006 0.3 0.1 1)
	(plenv -1.0 1.0 -1.0 1.0 0 0)
	(plcol0 2)
	(plcont z 1 xpts 1 ypts clevel 'x9-pltr-callback nil)
	(plstyl 1 mark space)
	(plcol0 3)
	(plcont w 1 xpts 1 ypts clevel 'x9-pltr-callback nil)
	(plstyl 0 mark space)
	(plcol0 1)
	(pllab "X Coordinate" "Y Coordinate" "Streamlines of flow")

	;; plot2
	(pl-setcontlabelparam 0.006 0.3 0.1 0)
	(plenv -1.0 1.0 -1.0 1.0 0 0)
	(plcol0 2)
	(with-pltr-data (pltr-data xg1 yg1 :z-vals z)
	  (plcont z 1 xpts 1 ypts clevel 'pltr1-callback pltr-data))
	(plstyl 1 mark space)
	(plcol0 3)
	(with-pltr-data (pltr-data xg1 yg1 :z-vals w)
	  (plcont w 1 xpts 1 ypts clevel 'pltr1-callback pltr-data))
	(plstyl 0 mark space)
	(plcol0 1)
	(pllab "X Coordinate" "Y Coordinate" "Streamlines of flow")	
  
	;; plot3
	(plenv -1.0 1.0 -1.0 1.0 0 0)
	(plcol0 2)
	(with-pltr-data (pltr-data gridx gridy :z-vals z)
	  (plcont z 1 xpts 1 ypts clevel 'pltr2-callback pltr-data))
	(plstyl 1 mark space)
	(plcol0 3)
	(with-pltr-data (pltr-data gridx gridy :z-vals w)
	  (plcont w 1 xpts 1 ypts clevel 'pltr2-callback pltr-data))
	(plstyl 0 mark space)
	(plcol0 1)
	(pllab "X Coordinate" "Y Coordinate" "Streamlines of flow")
	
	;; plot4
	(pl-setcontlabelparam 0.006 0.3 0.1 0)
	(polar)

	;; plot5
	(pl-setcontlabelparam 0.006 0.3 0.1 0)
	(potential)
	)))

  (plend1))

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
