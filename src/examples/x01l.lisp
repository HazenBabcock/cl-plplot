;;;;
;;;; PLplot example 1
;;;;
;;;; hazen 01/14
;;;;

(in-package :plplot-examples)

(defun example1 (&optional (dev default-dev))
  (format t "PLplot library version: ~a" (plgver))
  (plsdev dev)
  (plstar 2 2)
  
  ;; plots 1 & 2
  (labels ((plot (xscale yscale xoff yoff)
	     (let ((x (make-float-array 60))
		   (y (make-float-array 60))
		   (xs (make-float-array 6))
		   (ys (make-float-array 6)))
	       (dotimes (i 60)
		 (setf (aref x i) (+ xoff (/ (* xscale (+ i 1.0)) 60.0)))
		 (setf (aref y i) (+ yoff (* yscale (expt (aref x i) 2)))))
	       (dotimes (i 6)
		 (setf (aref xs i) (aref x (+ (* i 10) 3)))
		 (setf (aref ys i) (aref y (+ (* i 10) 3))))
	       (plcol0 1)
	       (plenv (aref x 0) (aref x 59) (aref y 0) (aref y 59) 0 0)
	       (plcol0 2)
	       (pllab "(x)" "(y)" "#frPLplot Example 1 - y=x#u2")
	       (plcol0 4)
	       (plpoin xs ys 9)
	       (plcol0 3)
	       (plline x y))))
    (plot 6.0 1.0 0.0 0.0)
    (plot 1.0 0.0014 0.0 0.0185))

  ;; plot 3
  (let ((x (make-float-array 100))
	(y (make-float-array 100)))
    (dotimes (i 100)
      (setf (aref x i) (/ (- i 19.0) 6.0))
      (setf (aref y i) 1.0)
      (when (/= (aref x i) 0.0)
	(setf (aref y i) (/ (sin (aref x i)) (aref x i)))))
    (plcol0 1)
    (plenv -2.0 10.0 -0.4 1.2 0 1)
    (plcol0 2)
    (pllab "(x)" "sin(x)/x" "#frPLplot Example 1 - Sinc Function")
    (plcol0 3)
    (plwidth 2)
    (plline x y)
    (plwidth 1))

  ;; plot 4
  (let ((x (make-float-array 101))
	(y (make-float-array 101))
	(mark0 (make-int-array 1 0))
	(space0 (make-int-array 1 0))
	(mark1 (make-int-array 1 1500))
	(space1 (make-int-array 1 1500)))
    (dotimes (i 101)
      (setf (aref x i) (* 3.6 i))
      (setf (aref y i) (sin (/ (* (aref x i) pi) 180.0))))
    (pladv 0)
    (plvsta)
    (plwind 0.0 360.0 -1.2 1.2)
    (plcol0 1)
    (plbox "bcnst" 60.0 2 "bcnstv" 0.2 2)
    (plstyl 1 mark1 space1)
    (plcol0 2)
    (plbox "g" 30.0 0 "g" 0.2 0)
    (plstyl 0 mark0 space0)
    (plcol0 3)
    (pllab "Angle (degrees)" "sine" "#frPLplot Example 1 - Sine function")
    (plcol0 4)
    (plline x y))

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
