;;;;
;;;; PLplot example 3
;;;;
;;;; hazen 06/10
;;;;

(in-package :plplot-examples)

(defun example3 (&optional (dev default-dev))
  (plsdev dev)
  (plsori 1)
  (plinit)
  (plenv -1.3 1.3 -1.3 1.3 1 -2)
  (let ((dtr (/ pi 180.0)))
    (dotimes (i 10)
      (plarc 0.0 0.0 (* 0.1 (+ i 1)) (* 0.1 (+ i 1)) 0.0 360.0 0.0 nil))
    (plcol0 2)
    (dotimes (i 12)
      (let* ((theta (* 30.0 i))
	     (dx (cos (* theta dtr)))
	     (dy (sin (* theta dtr)))
	     (offset (cond ((< theta 9.99) 0.45)
			   ((< theta 99.9) 0.30)
			   (t 0.15))))
	(pljoin 0.0 0.0 dx dy)
	(if (>= dx -0.00001)
	    (plptex dx dy dx dy (- offset) (write-to-string (round theta)))
	    (plptex dx dy (- dx) (- dy) (1+ offset) (write-to-string (round theta))))))
    (let ((x (make-float-array 361))
	  (y (make-float-array 361)))
      (dotimes (i 361)
	(let ((r (sin (* dtr 5.0 i))))
	  (setf (aref x i) (* r (cos (* i dtr)))
		(aref y i) (* r (sin (* i dtr))))))
      (plcol0 3)
      (plline x y)))
  (plcol0 4)
  (plmtex "t" 2.0 0.5 0.5 "#frPLplot Example 3 - r(#gh)=sin 5#gh")
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
