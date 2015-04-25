;;;;
;;;; PLplot example 8
;;;;
;;;; hazen 06/10
;;;;

(in-package :plplot-examples)

(defun example8 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (let ((alt (vector 60.0 40.0))
	(az (vector 30.0 -30.0))
	(title (vector "#frPLplot Example 8 - Alt=60, Az=30"
		       "#frPLplot Example 8 - Alt=40, Az=-30")))
    (labels ((cmap1-init (gray)
	       (let ((i (make-float-array 2))
		     (h (make-float-array 2))
		     (l (make-float-array 2))
		     (s (make-float-array 2)))
		 (setf (aref i 0) 0.0
		       (aref i 1) 1.0)
		 (if gray
		     (setf (aref h 0) 0.0
			   (aref h 1) 0.0
			   (aref l 0) 0.5
			   (aref l 1) 1.0
			   (aref s 0) 0.0
			   (aref s 1) 0.0)
		     (setf (aref h 0) 240.0
			   (aref h 1) 0.0
			   (aref l 0) 0.6
			   (aref l 1) 0.6
			   (aref s 0) 0.8
			   (aref s 1) 0.8))
		 (plscmap1n 256)
		 (plscmap1l nil i h l s nil))))
      (let* ((xpts 35)
	     (ypts 45)
	     (rosen nil)
	     (clevel (make-float-array 10))
	     (dx (/ 2.0 (- xpts 1)))
	     (dy (/ 2.0 (- ypts 1)))
	     (x (make-float-array xpts))
	     (y (make-float-array ypts))
	     (z (make-float-array (list xpts ypts)))
	     (z-row-major (make-float-array (* xpts ypts)))
	     (z-col-major (make-float-array (* ypts xpts)))
	     (x0 (* 0.5 (- xpts 1)))
	     (a (* 0.9 x0))
	     (y0 (* 0.5 (- ypts 1)))
	     (b (* 0.7 y0))
	     (indexymin (make-float-array xpts))
	     (indexymax (make-float-array xpts))
	     (zlimited (make-float-array (list xpts ypts))))
	(dotimes (i xpts)
	  (setf (aref x i) (+ -1.0 (* i dx)))
	  (when rosen
	    (setf (aref x i) (* (aref x i) 1.5))))
	(dotimes (i ypts)
	  (setf (aref y i) (+ -1.0 (* i dy)))
	  (when rosen
	    (incf (aref y i) 0.5)))
	(dotimes (i xpts)
	  (dotimes (j ypts)
	    (if rosen
		(progn
		  (setf (aref z i j) (+ (expt (- 1.0 (aref x i)) 2.0)
					(* 100.0 (expt (- (aref y j)
							  (expt (aref x i) 2.0))
						       2.0))))
		  (setf (aref z i j) (if (= 0.0 (aref z i j))
					 -5.0
					 (log (aref z i j)))))
		(let ((r (sqrt (+ (* (aref x i) (aref x i)) (* (aref y j) (aref y j))))))
		  (setf (aref z i j) (* (exp (* -1.0 r r)) (cos (* 2.0 pi r))))))
	    (setf (aref z-row-major (+ (* i ypts) j)) (aref z i j)
		  (aref z-col-major (+ i (* j xpts))) (aref z i j))))
	(dotimes (i xpts)
	  (let ((square-root (sqrt (- 1.0 (min 1.0 (expt (/ (- i x0) a) 2.0))))))
	    (setf (aref indexymin i) (max 0 (floor (+ 0.5 y0 (- (* b square-root)))))
		  (aref indexymax i) (min ypts (+ 1 (floor (+ 0.5 y0 (* b square-root))))))
	    (do ((j (aref indexymin i) (1+ j)))
		((>= j (aref indexymax i)) t)
	      (setf (aref zlimited i j) (aref z i j)))))
	(multiple-value-bind (zmax zmin) (plminmax2dgrid z)
	  (let ((step (/ (- zmax zmin) 11.0)))
	    (dotimes (i 10)
	      (setf (aref clevel i) (+ zmin step (* step i)))))
	  (pllightsource 1.0 1.0 1.0)
	  (dotimes (k 2)
	    (dotimes (ifshade 5)
	      (pladv 0)
	      (plvpor 0.0 1.0 0.0 0.9)
	      (plwind -1.0 1.0 -0.9 1.1)
	      (plcol0 3)
	      (plmtex "t" 1.0 0.5 0.5 (aref title k))
	      (plcol0 1)
	      (if rosen
		  (plw3d 1.0 1.0 1.0 -1.5 1.5 -0.5 1.5 zmin zmax (aref alt k) (aref az k))
		  (plw3d 1.0 1.0 1.0 -1.0 1.0 -1.0 1.0 zmin zmax (aref alt k) (aref az k)))
	      (plbox3 "bnstu" "x axis" 0.0 0
		      "bnstu" "y axis" 0.0 0
		      "bcdmnstuv" "z axis" 0.0 0)
	      (plcol0 2)
	      (cond
		((= ifshade 0)
		 (progn
		   (cmap1-init t)
		   (with-foreign-matrix (z foreign-z)
		     (plfsurf3d x
				y
				(plf2ops-c)
				foreign-z
				0
				nil))))
		((= ifshade 1)
		 (progn
		   (cmap1-init nil)
		   (with-foreign-grid (z foreign-z xpts ypts)
		     (plfsurf3d x
				y
				(plf2ops-grid-c)
				foreign-z
				mag-color
				nil))))
		((= ifshade 2)
		 (progn
		   (cmap1-init nil)
		   (with-foreign-grid (z-row-major foreign-z-row-major xpts ypts)
		     (plfsurf3d x
				y
				(plf2ops-grid-row-major)
				foreign-z-row-major
				(+ mag-color faceted)
				nil))))
		((= ifshade 3)
		 (progn
		   (cmap1-init nil)
		   (with-foreign-grid (z-col-major foreign-z-col-major xpts ypts)
		     (plfsurf3d x
				y
				(plf2ops-grid-col-major)
				foreign-z-col-major
				(+ mag-color surf-cont base-cont)
				clevel))))
		(t
		 (progn
		   (cmap1-init nil)
		   (plsurf3dl x
			      y
			      zlimited
			      (+ mag-color surf-cont base-cont)
			      clevel
			      0
			      xpts
			      indexymin
			      indexymax))))))))))
  (plend1))

;;;;
;;;; Copyright (c) 2010 Hazen P. Babcock
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
