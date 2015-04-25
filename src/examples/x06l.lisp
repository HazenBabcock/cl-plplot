;;;;
;;;; PLplot example 6
;;;;
;;;; hazen 06/10
;;;;

(in-package :plplot-examples)

(defun example6 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (dotimes (kind-font 2)
    (plfontld kind-font)
    (let ((max-font (if (= kind-font 0) 1 4)))
      (dotimes (font max-font)
	(plfont (1+ font))
	(pladv 0)
	(plcol0 2)
	(plvpor 0.1d0 1.0d0 0.1d0 0.9d0)
	(plwind 0.0 1.0 0.0 1.3)
	(plbox "bcg" 0.1 0 "bcg" 0.1 0)
	(plcol0 15)
	(dotimes (i 10)
	  (plmtex "b" 1.5 (+ (* 0.1 i) 0.05) 0.5 (write-to-string i)))
	(let ((k 0))
	  (dotimes (i 13)
	    (plmtex "lv" 1.0 (- 1.0 (/ (+ (* 2.0 i) 1.0) 26.0)) 1.0 (write-to-string (* 10 i)))
	    (dotimes (j 10)
	      (let ((x (make-float-array 1))
		    (y (make-float-array 1)))
		(setf (aref x 0) (+ (* j 0.1) 0.05)
		      (aref y 0) (- 1.25 (* i 0.1)))
		(when (< k 128)
		  (plpoin x y k))
		(incf k)))))
	(if (= kind-font 0)
	    (plmtex "t" 1.5 0.5 0.5 "PLplot Example 6 - plpoin symbols (compact)")
	    (plmtex "t" 1.5 0.5 0.5 "PLplot Example 6 - plpoin symbols (extended)")))))
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
