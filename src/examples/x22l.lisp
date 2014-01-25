;;;;
;;;; PLplot example 22
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example22 (&optional (dev default-dev))
  (plsdev dev)
  (let ((arrow-x (vector -0.5 0.5 0.3 0.5 0.3 0.5))
	(arrow-y (vector 0.0 0.0 0.2 0.0 -0.2 0.0))
	(arrow2-x (vector -0.5 0.3 0.3 0.5 0.3 0.3))
	(arrow2-y (vector 0.0 0.0 0.2 0.0 -0.2 0.0)))
    (labels ((circulation ()
	       (let* ((nx 20)
		      (ny 20)
		      (cgrid2-x (make-float-array (list nx ny)))
		      (cgrid2-y (make-float-array (list nx ny)))
		      (u (make-float-array (list nx ny)))
		      (v (make-float-array (list nx ny))))
		 (dotimes (i nx)
		   (let ((x (- i (/ nx 2.0) -0.5)))
		     (dotimes (j ny)
		       (let ((y (- j (/ ny 2.0) -0.5)))
			 (setf (aref cgrid2-x i j) x
			       (aref cgrid2-y i j) y
			       (aref u i j) y
			       (aref v i j) (- x))))))
		 (plenv (/ (- nx) 2.0) (/ nx 2.0) (/ (- ny) 2.0) (/ ny 2.) 0 0)
		 (pllab "(x)" "(y)" "#frPLplot Example 22 - circulation")
		 (plcol0 2)
		 (pl-set-pltr-fn #'pltr2)
		 (plvect u v 0.0 cgrid2-x cgrid2-y)
		 (pl-reset-pltr-fn)
		 (plcol0 1)))
	     (constriction ()
	       (let* ((nx 20)
		      (ny 20)
		      (xmax (/ nx 2.0))
		      (ymax (/ ny 2.0))
		      (cgrid2-x (make-float-array (list nx ny)))
		      (cgrid2-y (make-float-array (list nx ny)))
		      (u (make-float-array (list nx ny)))
		      (v (make-float-array (list nx ny))))
		 (dotimes (i nx)
		   (let ((x (- i (/ nx 2.0) -0.5)))
		     (dotimes (j ny)
		       (let ((y (- j (/ ny 2.0) -0.5))
			     (b (* (/ ymax 4.0) (- 3.0 (cos (/ (* 3.14159 x) xmax))))))
			 (setf (aref cgrid2-x i j) x
			       (aref cgrid2-y i j) y)
			 (if (< (abs y) b)
			     (let ((dbdx (* (/ ymax 4.0)
					    (sin (/ (* 3.14159 x) xmax))
					    (/ y b))))
			       (setf (aref u i j) (/ (* 2.0 ymax) b)
				     (aref v i j) (* dbdx (aref u i j))))
			     (setf (aref u i j) 0.0
				   (aref v i j) 0.0))))))
		 (plenv (/ (- nx) 2.0) xmax (/ (- ny) 2.0) ymax 0 0)
		 (pllab "(x)" "(y)" "#frPLplot Example 22 - constriction")
		 (plcol0 2)
		 (pl-set-pltr-fn #'pltr2)
		 (plvect u v -0.5 cgrid2-x cgrid2-y)
		 (pl-reset-pltr-fn)
		 (plcol0 1)))
	     (potential ()
	       (let* ((nper 100)
		      (nlevel 10)
		      (nr 20)
		      (ntheta 20)
		      (cgrid2-x (make-float-array (list nr ntheta)))
		      (cgrid2-y (make-float-array (list nr ntheta)))
		      (u (make-float-array (list nr ntheta)))
		      (v (make-float-array (list nr ntheta)))
		      (z (make-float-array (list nr ntheta))))
		 (let* ((rmax nr)
			(eps 2.0)
			(q1 1.0)
			(d1 (/ rmax 4.0))
			(q1i (/ (* (- q1) rmax) d1))
			(d1i (/ (expt rmax 2.0) d1))
			(q2 -1.0)
			(d2 (/ rmax 4.0))
			(q2i (/ (* (- q2) rmax) d2))
			(d2i (/ (expt rmax 2.0) d2)))
		   (dotimes (i nr)
		     (let ((r (+ 0.5 i)))
		       (dotimes (j ntheta)
			 (let* ((theta (* 2.0 (/ 3.14159 (1- ntheta)) (+ 0.5 j)))
				(x (* r (cos theta)))
				(y (* r (sin theta)))
				(div1 (sqrt (+ (expt (- x d1) 2.0) (expt (- y d1) 2.0) (expt eps 2.0))))
				(div1i (sqrt (+ (expt (- x d1i) 2.0) (expt (- y d1i) 2.0) (expt eps 2.0))))
				(div2 (sqrt (+ (expt (- x d2) 2.0) (expt (+ y d2) 2.0) (expt eps 2.0))))
				(div2i (sqrt (+ (expt (- x d2i) 2.0) (expt (+ y d2i) 2.0) (expt eps 2.0)))))
			   (setf (aref cgrid2-x i j) x
				 (aref cgrid2-y i j) y
				 (aref z i j) (+ (/ q1 div1) (/ q1i div1i) (/ q2 div2) (/ q2i div2i))
				 (aref u i j) (- (/ (* (- q1) (- x d1)) (expt div1 3.0))
						 (/ (* q1i (- x d1i)) (expt div1i 3.0))
						 (/ (* q2 (- x d2)) (expt div2 3.0))
						 (/ (* q2i (- x d2i) (expt div2i 3.0))))
				 (aref v i j) (- (/ (* (- q1) (- y d1)) (expt div1 3.0))
						 (/ (* q1i (- y d1i)) (expt div1i 3.0))
						 (/ (* q2 (+ y d2)) (expt div2 3.0))
						 (/ (* q2i (+ y d2i) (expt div2i 3.0)))))))))
		   (multiple-value-bind (xmin xmax) (min-max cgrid2-x)
		     (multiple-value-bind (ymin ymax) (min-max cgrid2-y)
		       (multiple-value-bind (zmin zmax) (min-max z)
			 (plenv xmin xmax ymin ymax 0 0)
			 (pllab "(x)" "(y)" "#frPLplot Example 22 - potential gradient vector plot")
			 (plcol0 3)
			 (pllsty 2)
			 (pl-set-pltr-fn #'pltr2)
			 (let ((dz (/ (- zmax zmin) nlevel))
			       (clevel (make-float-array nlevel)))
			   (dotimes (i nlevel)
			     (setf (aref clevel i) (+ zmin (* (+ i 0.5) dz))))
			   (plcont z 1 nr 1 ntheta clevel cgrid2-x cgrid2-y))
			 (pllsty 1)
			 (plcol0 2)
			 (plvect u v 25.0 cgrid2-x cgrid2-y)
			 (pl-reset-pltr-fn)
			 (plcol0 1)
			 (let ((px (make-float-array nper))
			       (py (make-float-array nper)))
			   (dotimes (i nper)
			     (let ((theta (* 2.0 i (/ 3.14159 (- nper 1.0)))))
			       (setf (aref px i) (* rmax (cos theta))
				     (aref py i) (* rmax (sin theta)))))
			   (plline px py)))))))))

      (plinit)
      (circulation)
      (plsvect arrow-x arrow-y 0)
      (constriction)
      (plsvect arrow2-x arrow2-y 1)
      (constriction)
      (potential)
      (plend1))))

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
