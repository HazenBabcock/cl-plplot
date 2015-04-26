;;;;
;;;; PLplot example 11
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example11 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (plscmap1n 256)
  (plscmap1l nil (vector 0.0 1.0) (vector 240 0) (vector 0.6 0.6) (vector 0.8 0.8) nil)

  (let* ((xpts 35)
	 (ypts 46)
	 (levels 10)
	 (opt (vector 3 3))
	 (alt (vector 33.0 17.0))
	 (az (vector 24.0 115.0))
	 (title (vector "#frPLplot Example 11 - Alt=33, Az=24, Opt=3"
			"#frPLplot Example 11 - Alt=17, Az=115, Opt=3"))
	 (x (make-float-array xpts))
	 (y (make-float-array ypts))
	 (z (make-float-array (list xpts ypts)))
	 (clevel (make-float-array levels)))
    (dotimes (i xpts)
      (setf (aref x i) (* 3.0 (/ (- i (floor (/ xpts 2))) (floor (/ xpts 2.0))))))
    (dotimes (i ypts)
      (setf (aref y i) (* 3.0 (/ (- i (/ ypts 2.0)) (/ ypts 2.0)))))
    (dotimes (i xpts)
      (let ((xx (aref x i)))
	(dotimes (j ypts)
	  (let ((yy (aref y j)))
	    (setf (aref z i j)(- (* 3.0 (- 1.0 xx) (- 1.0 xx) (exp (- (* xx xx -1.0) (* (+ yy 1.0) (+ yy 1.0)))))
				 (* 10.0 (- (/ xx 5.0) (expt xx 3.0) (expt yy 5.0)) (exp (- (* xx xx -1.0) (* yy yy))))
				 (* (/ 1.0 3.0) (exp (- (* (+ xx 1.0) (+ xx 1.0) -1.0) (* yy yy))))))))))
    
    (multiple-value-bind (zmin zmax) (min-max z)
      (let ((step (/ (- zmax zmin) (+ levels 1.0))))
	(dotimes (i levels)
	  (setf (aref clevel i) (+ zmin step (* step i)))))
      (dotimes (k 2)
	(dotimes (i 4)
	  (pladv 0)
	  (plcol0 1)
	  (plvpor 0.0 1.0 0.0 0.9)
	  (plwind -1.0 1.0 -1.0 1.5)
	  (plw3d 1.0 1.0 1.2 -3.0 3.0 -3.0 3.0 zmin zmax (aref alt k) (aref az k))
	  (plbox3 "bnstu" "x axis" 0.0 0
		  "bnstu" "y axis" 0.0 0
		  "bcdmnstuv" "z axis" 0.0 4)
	  (plcol0 2)
	  (cond
	    ((= i 0)
	     (plmesh x y z (aref opt k)))
	    ((= i 1)
	     (plmesh x y z (+ (aref opt k) mag-color)))
	    ((= i 2)
	     (plot3d x y z (+ (aref opt k) mag-color) 1))
	    ((= i 3)
	     (plmeshc x y z (+ (aref opt k) mag-color base-cont) clevel)))
	  (plcol0 3)
	  (plmtex "t" 1.0 0.5 0.5 (aref title k))))))

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
