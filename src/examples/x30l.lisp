;;;;
;;;; PLplot example 30
;;;;
;;;; hazen 08/10
;;;;

(in-package :plplot-examples)

(defun example30 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (plscmap0n 4)
  (plscmap0a (vector 0 255 0 0)
	     (vector 0 0 255 0)
	     (vector 0 0 0 255)
	     (vector 1.0 1.0 1.0 1.0))

  ; plot1
  (pladv 0)
  (plvpor 0.0 1.0 0.0 1.0)
  (plwind 0.0 1.0 0.0 1.0)
  (plcol0 0)
  (plbox "" 1.0 0 "" 1.0 0)
  (let ((px (vector 0.1 0.5 0.5 0.1))
	(py (vector 0.1 0.1 0.5 0.5)))
    (dotimes (i 9)
      (let ((icol (1+ (mod i 3))))
	(multiple-value-bind (r g b a) (plgcol0a icol)
	  (declare (ignore a))
	  (plscol0a icol r g b (- 1.0 (/ i 9.0))))
	(plcol0 icol)
	(plfill px py)
	(dotimes (j 4)
	  (incf (aref px j) (/ 0.5 9.0))
	  (incf (aref py j) (/ 0.5 9.0))))))

  ; plot2
  (pladv 0)
  (plvpor 0.1 0.9 0.1 0.9)
  (plwind 0.0 1.0 0.0 1.0)
  (dotimes (i 5)
    (let ((icol (1+ (mod i 3)))
	  (px (make-float-array 4)))
      (setf (aref px 0) (+ 0.05 (* 0.2 i))
	    (aref px 1) (+ (aref px 0) 0.1)
	    (aref px 2) (aref px 1)
	    (aref px 3) (aref px 0))
      (multiple-value-bind (r g b a) (plgcol0a icol)
	(declare (ignore a))
	(plscol0a icol r g b 1.0))
      (plcol0 icol)
      (dotimes (j 5)
	(let ((py (make-float-array 4)))
	  (setf (aref py 0) (+ 0.05 (* 0.2 j))
		(aref py 1) (aref py 0)
		(aref py 2) (+ (aref py 0) 0.1)
		(aref py 3) (aref py 2))
	  (plfill px py)))))
  (plscmap1n 128)
  (plscmap1la t
	      (vector 0.0 1.0)
	      (vector 1.0 1.0)
	      (vector 0.0 0.0)
	      (vector 0.0 0.0)
	      (vector 0.0 1.0)
	      (vector nil nil))
  (plgradient (vector 0.0 1.0 1.0 0.0)
	      (vector 0.0 0.0 1.0 1.0)
	      90.0)
  
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
