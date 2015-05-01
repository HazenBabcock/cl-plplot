;;;;
;;;; PLplot example 21
;;;;
;;;; This example *may* have some SBCL specific
;;;; handling of NaN?
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example21 (&optional (dev default-dev))
  (plsdev dev)
  (let ((knn-order 20)
	(threshold 1.001)
	(wmin -1.0e3)
	(randn nil)
	(rosen nil)
	(xmin -0.2)
	(ymin -0.2)
	(xmax 0.6)
	(ymax 0.6))
    (labels ((create-grid (xp yp)
	       (let ((x (make-float-array xp))
		     (y (make-float-array yp)))
		 (dotimes (i xp)
		   (setf (aref x i) (+ xmin (/ (* (- xmax xmin) i) (- xp 1)))))
		 (dotimes (i yp)
		   (setf (aref y i) (+ ymin (/ (* (- ymax ymin) i) (- yp 1)))))
		 (values x y)))
	     (create-data (pts)
	       (let ((xi (make-float-array pts))
		     (yi (make-float-array pts))
		     (zi (make-float-array pts)))
		 (dotimes (i pts)
		   (let ((xt (* (- xmax xmin) (plrandd)))
			 (yt (* (- ymax ymin) (plrandd))))
		     (if (not randn)
			 (setf (aref xi i) (+ xt xmin)
			       (aref yi i) (+ yt ymin))
			 (setf (aref xi i) (+ (* (sqrt (* -2.0 (log xt)))
						 (cos (* 2.0 pi yt)))
					      xmin)
			       (aref yi i) (+ (* (sqrt (* -2.0 (log xt)))
						 (sin (* 2.0 pi yt)))
					      ymin)))
		     (if (not rosen)
			 (let ((r (sqrt (+ (* (aref xi i) (aref xi i))
					   (* (aref yi i) (aref yi i))))))
			   (setf (aref zi i) (* (exp (* -1.0 r r))
						(cos (* 2.0 pi r)))))
			 (setf (aref zi i) (log (+ (expt (- 1.0 (aref xi i)) 2.0)
						   (* 100.0 (expt (- (aref yi i)
								     (expt (aref xi i) 2.0)) 2.0))))))))
		 (values xi yi zi)))
	     (nan-p (x)
	       (handler-case
		   (/= x x)
		 (error (se) t))))
      (let ((title (vector "Cubic Spline Approximation"
			   "Delaunay Linear Interpolation"
			   "Natural Neighbors Interpolation"
			   "KNN Inv. Distance Weighted"
			   "3NN Linear Interpolation"
			   "4NN Around Inv. Dist. Weighted"))
	    (opt (vector 0.0
			 0.0
			 wmin
			 knn-order
			 threshold
			 0.0))
	    (clev (make-float-array 16))
	    (xp 25)
	    (yp 20)
	    (nl 16)
	    (pts 500))
	(plinit)
	(plscmap1n 256)
	(plscmap1l nil (vector 0.0 1.0) (vector 240.0 0.0) (vector 0.6 0.6) (vector 0.8 0.8) nil)
	(plseed 5489)
	(multiple-value-bind (x y z) (create-data pts)
	  (multiple-value-bind (zmin zmax) (min-max z)
	    (multiple-value-bind (xg yg) (create-grid xp yp)
	      (plcol0 1)
	      (plenv xmin xmax ymin ymax 2 0)
	      (plcol0 15)
	      (pllab "X" "Y" "The original data sampling")
	      (dotimes (i pts)
		(plcol1 (/ (- (aref z i) zmin) (- zmax zmin)))
		(plstring (vector (aref x i)) (vector (aref y i)) "#(727)"))
	      (pladv 0)

	      (plssub 3 2)
	      (dotimes (k 2)
		(pladv 0)
		(dotimes (alg 6)
		  (let ((zg (plgriddata x y z xg yg (1+ alg) (aref opt alg))))
		    ; this part deals with NaNs by averaging non NaN neighbors.
		    (dotimes (i xp)
		      (dotimes (j yp)
			(when (nan-p (aref zg i j))
			  ; why isn't the coercion automatic?
			  (setf (aref zg i j) (coerce 0.0 'double-float))
			  (let ((dist 0.0)
				(ii (- i 1)))
			    (do () ((or (> ii (+ i 1)) (>= ii xp)))
			      (let ((jj (- j 1)))
				(do () ((or (> jj (+ j 1)) (>= jj yp)))
				  (when (and (>= ii 0)
					     (>= jj 0)
					     (not (nan-p (aref zg ii jj))))
				    (let ((d (+ (abs (- ii i))
						(abs (- jj j)))))
				      (when (/= d 1.0)
					(setf d 1.4142))
				      (incf (aref zg i j) (/ (aref zg ii jj) (* d d)))
				      (incf dist d)))
				  (incf jj)))
			      (incf ii))
			    (setf (aref zg i j)
				  (if (/= dist 0.0)
				      (/ (aref zg i j) dist)
				      zmin))))))
		    (multiple-value-bind (lzmin lzmax) (min-max zg)
		      (setf lzmin (- (if (< lzmin zmin) lzmin zmin) 0.01)
			    lzmax (+ (if (> lzmax zmax) lzmax zmax) 0.01))
		      (plcol0 1)
		      (pladv (1+ alg))
		      (dotimes (i nl)
			(setf (aref clev i) (+ lzmin (* (/ (- lzmax lzmin) (- nl 1)) i))))
		      (if (= k 0)
			  (progn
			    (plenv0 xmin xmax ymin ymax 2 0)
			    (plcol0 15)
			    (pllab "X" "Y" (aref title alg))
			    (plshades zg nil
				      xmin xmax ymin ymax 
				      clev 1 0 1
				      'plfill-callback
				      t nil nil)
			    (plcol0 2))
			  (progn
			    ;(plscmap1n 256)
			    ;(plscmap1l nil (vector 0.0 1.0) (vector 240 0) (vector 0.6 0.6) (vector 0.8 0.8) nil)
			    (plvpor 0.0 1.0 0.0 0.9)
			    (plwind -1.1 0.75 -0.65 1.20)
			    (plw3d 1 1 1 xmin xmax ymin ymax lzmin lzmax 30 -40)
			    (plbox3 "bntu" "X" 0.0 0
				    "bntu" "Y" 0.0 0
				    "bcdfntu" "Z" 0.5 0)
			    (plcol0 15)
			    (pllab "" "" (aref title alg))
			    (plot3dc xg yg zg (+ 3 (ash 1 2) (ash 1 3)) clev))))))))))
	(plend1)))))

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
