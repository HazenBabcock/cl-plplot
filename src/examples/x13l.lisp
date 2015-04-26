;;;;
;;;; PLplot example 13
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example13 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (pladv 0)
  (plvasp 1.0)
  (plwind 0.0 10.0 0.0 10.0)
  (plcol0 2)
  (let ((text (vector "Maurice" "Geoffrey" "Alan" "Rafael" "Vince"))
	(x (make-float-array 500))
	(y (make-float-array 500))
	(per (vector 10.0 32.0 12.0 30.0 16.0))
	(theta0 0.0)
	(dthet 1.0))
    (dotimes (i 5)
      (let ((j 0)
	    (theta1 (floor (+ theta0 (* 5.0 (aref per i))))))
	(setf (aref x j) 5.0
	      (aref y j) 5.0)
	(incf j)
	(when (= i 4)
	  (setf theta1 500))
	(do ((theta theta0 (+ theta dthet)))
	    ((> theta theta1))
	  (setf (aref x j) (+ 5.0 (* 3.0 (cos (* (/ (* 2.0 pi) 500.0) theta))))
		(aref y j) (+ 5.0 (* 3.0 (sin (* (/ (* 2.0 pi) 500.0) theta)))))
	  (incf j))
	(plcol0 (+ i 1))
	(plpsty (+ (mod (+ i 3) 8) 1))
	(let ((tx (adjust-array x j))
	      (ty (adjust-array y j)))
	  (plfill tx ty)
	  (plcol0 1)
	  (plline tx ty))
	(let* ((just (* (/ (* 2.0 pi) 500.0) (/ (+ theta0 theta1) 2.0)))
	       (dx (* 0.25 (cos just)))
	       (dy (* 0.25 (sin just))))
	  (if (or (< (+ theta0 theta1) 250)
		  (> (+ theta0 theta1) 750))
	      (setf just 0.0)
	      (setf just 1.0))
	  (plptex (+ (aref x (floor j 2)) dx)
		  (+ (aref y (floor j 2)) dy)
		  1.0 0.0 just (aref text i)))
	;(setf theta0 (- theta1 dthet))
	(setf theta0 theta1)
	)))
  (plfont 2)
  (plschr 0.0 1.3)
  (plptex 5.0 9.0 1.0 0.0 0.5 "Percentage of Sales")

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
