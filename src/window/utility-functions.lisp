;;;;
;;;; Copyright (c) 2006 Hazen P. Babcock
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
;;;; 
;;;; This file contains a number of utility functions that are used/useful
;;;; for plot generation.
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(defun make-float-vector (vector-length)
  "Returns an array of floating point numbers."
  (make-array vector-length :initial-element 0.0 :element-type 'float))

(defun make-index-vector (vector-length)
  "Creates a vector of numbers 1,2,..,vector-length."
  (let ((index-vector (make-float-vector vector-length)))
    (dotimes (i vector-length)
      (setf (aref index-vector i) (1+ i)))
    index-vector))

; warning re. out of range value?
(defun range-check (val min max)
  "Returns true if val is between min and max (inclusive)."
  (and (>= val min)
       (<= val max)))

(defun copy-vector (vector copy?)
  "If desired, makes a copy of a vector."
  (if copy?
      (copy-seq vector)
      vector))

(defun copy-matrix (matrix copy?)
  "If desired, makes a copy of a matrix."
  (if (= (array-rank matrix) 1)
      (copy-vector matrix copy?)
      (if copy?
	  (let ((new-matrix (make-array (list (array-dimension matrix 0)
					      (array-dimension matrix 1))
					:initial-element 0.0
					:element-type 'float)))
	    (dotimes (i (array-dimension matrix 0))
	      (dotimes (j (array-dimension matrix 1))
		(setf (aref new-matrix i j) (aref matrix i j))))
	    new-matrix)
	  matrix)))

