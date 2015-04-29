;;;;
;;;; PLplot example 15
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

; This doesn't actually do anything, you could also use nil in the call to plshade, 
; but it is provided as an example of how you might use a callback in plshade.
(cffi:defcallback x15-defined-fn-callback plint ((x plflt) (y plflt))
  (declare (ignore x y))
  1)

(defun example15 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)

  (let* ((xpts 35)
	 (ypts 46)
	 (x (make-float-array xpts))
	 (y (make-float-array ypts))
	 (z (make-float-array (list xpts ypts))))
    (dotimes (i xpts)
      (let ((xx (/ (- i (floor (/ xpts 2.0))) (floor (/ xpts 2.0)))))
	(setf (aref x i) (* (+ xx (/ 1.0 xpts)) 
			    (/ (+ xpts 1.0) xpts)))
	(dotimes (j ypts)
	  (let ((yy (- (/ (- j (/ ypts 2.0)) (/ ypts 2.0)) 1.0)))
	    (setf (aref y j) (* (+ yy 1.0 (/ 1.0 ypts))
				(/ (+ ypts 1.0) ypts)))
	    (setf (aref z i j) (+ (- (* xx xx) 
				     (* yy yy))
				  (/ (- xx yy)
				     (+ (* xx xx) (* yy yy) 0.1))))))))
    (multiple-value-bind (zmin zmax) (min-max z)
      (labels ((plot1 ()
		 (pladv 0)
		 (plvpor 0.1 0.9 0.1 0.9)
		 (plwind -1.0 1.0 -1.0 1.0)
		 (plpsty 8)
		 (plshade z
			  'x15-defined-fn-callback
			  -1.0 1.0 -1.0 1.0
			  (+ zmin (* (- zmax zmin) 0.4)) 
			  (+ zmin (* (- zmax zmin) 0.6))
			  0 7 2 
			  9 2 2 2
			  'plfill-callback
			  t
			  nil nil)
		 (plcol0 1)
		 (plbox "bcnst" 0.0 0 "bcnstv" 0.0 0)
		 (plcol0 2)
		 (pllab "distance" "altitude" "Bogon flux"))

	       (plot2 ()
		 (let ((nlin (vector 1 1 1 1 1 2 2 2 2 2))
		       (inc1 (vector 450 -450 0 900 300 450 0 0 450 0))
		       (inc2 (vector 0 0 0 0 0 -450 900 450 -450 900))
		       (del1 (vector 2000 2000 2000 2000 2000
				     2000 2000 2000 4000 4000))
		       (del2 (vector 2000 2000 2000 2000 2000
				     2000 2000 2000 4000 2000)))
		   (pladv 0)
		   (plvpor 0.1 0.9 0.1 0.9)
		   (plwind -1.0 1.0 -1.0 1.0)
		   (dotimes (i 10)
		     (plpat (if (= (aref nlin i) 1) 
				(vector (aref inc1 i))
				(vector (aref inc1 i) (aref inc2 i)))
			    (if (= (aref nlin i) 1)
				(vector (aref del1 i))
				(vector (aref del1 i) (aref del2 i))))
		     (plshade1 z
			       nil
			       -1.0 1.0 -1.0 1.0
			       (+ zmin (* (- zmax zmin) (/ i 10.0)))
			       (+ zmin (* (- zmax zmin) (/ (+ i 1.0) 10.0)))
			       0 (+ i 6)
			       2 0 0 0 0 
			       'plfill-callback 
			       t
			       nil nil))
		   (plcol0 1)
		   (plbox "bcnst" 0.0 0 "bcnstv" 0.0 0)
		   (plcol0 2)
		   (pllab "distance" "altitude" "Bogon flux")))

	       (plot3 ()
		 (let ((xx0 (vector -1.0 1.0 1.0 -1.0 -1.0))
		       (xx1 (vector -1.0 1.0 1.0 -1.0 -1.0))
		       (yy0 (vector 1.0 1.0 0.0 0.0 1.0))
		       (yy1 (vector -1.0 -1.0 0.0 0.0 -1.0))
		       (zz0 (vector 0.0 0.0 1.0 1.0 0.0))
		       (zz1 (vector 0.0 0.0 1.0 1.0 0.0)))
		   (pladv 0)
		   (plvpor 0.1 0.9 0.1 0.9)
		   (plwind -1.0 1.0 -1.0 1.0)
		   (plw3d 1.0 1.0 1.0 -1.0 1.0 -1.0 1.0 0.0 1.5 30 -40)
		   (plcol0 1)
		   (plbox3 "bntu" "X" 0.0 0 "bntu" "Y" 0.0 0 "bcdfntu" "Z" 0.5 0)
		   (plcol0 2)
		   (pllab "" "" "3-d polygon filling")
		   (plcol0 3)
		   (plpsty 1)
		   (plline3 xx0 yy0 zz0)
		   (plfill3 xx0 yy0 zz0)
		   (plpsty 2)
		   (plline3 xx1 yy1 zz1)
		   (plfill3 xx1 yy1 zz1))))

	;; main
	(plscmap1l nil
		   (vector 0.0 0.45 0.55 1.0)
		   (vector 260 260 20 20)
		   (vector 0.6 0.0 0.0 0.6)
		   (vector 1 0.5 0.5 1)
		   nil)
	(plot1)
	(plot2)
	(plot3))))

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
