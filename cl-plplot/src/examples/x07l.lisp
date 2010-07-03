;;;;
;;;; PLplot example 7
;;;;
;;;; hazen 06/10
;;;;

(in-package :plplot-examples)

(defun example7 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (plfontld 0)
  (dotimes (l 20)
    (when (= l 2)
      (plfontld 1))
    (pladv 0)
    (plcol0 2)
    (plvpor 0.15 0.95 0.1 0.9)
    (plwind 0.0 1.0 0.0 1.0)
    (plbox "bcg" 0.1 0 "bcg" 0.1 0)
    (plcol0 15)
    (dotimes (i 10)
      (plmtex "b" 1.5 (+ (* 0.1 i) 0.05) 0.5 (write-to-string i)))
    (let ((k 0)
	  (base (vector 0 100 0 100 200 500 600 700 800 900
			2000 2100 2200 2300 2400 2500 2600 2700 2800 2900)))
      (dotimes (i 10)
	(plmtex "lv" 1.0 (- 0.95 (* 0.1 i)) 1.0 (write-to-string (+ (aref base l) (* 10 i))))
	(dotimes (j 10)
	  (let ((x (make-float-array 1))
		(y (make-float-array 1)))
	    (setf (aref x 0) (+ 0.05 (* 0.1 j))
		  (aref y 0) (- 0.95 (* 0.1 i)))
	    (plsym x y (+ (aref base l) k))
	    (incf k)))))
    (if (< l 2)
	(plmtex "t" 1.5 0.5 0.5 "PLplot Example 7 - PLSYM symbols (compact)")
	(plmtex "t" 1.5 0.5 0.5 "PLplot Example 7 - PLSYM symbols (extended)")))
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
