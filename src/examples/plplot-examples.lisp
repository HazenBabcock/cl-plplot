;;;;
;;;; Lisp versions of the standard PLplot examples. This
;;;; particular file mostly just defines the package.
;;;;
;;;; hazen 06/10
;;;;


(defpackage :plplot-examples
  (:use :common-lisp
	:cl-plplot-system))

(in-package :plplot-examples)

(defparameter default-dev "xwin")

(defun make-float-array (length &optional (initial-value 0.0))
  (make-array length :element-type 'float :initial-element initial-value))

(defun make-int-array (length &optional (initial-value 0))
  (make-array length :element-type 'integer :initial-element initial-value))

(defun min-max (matrix)
  (if (= (array-rank matrix) 1)
      (let ((m-min (aref matrix 0))
	    (m-max (aref matrix 0)))
	(dotimes (i (array-dimension matrix 0))
	  (when (< (aref matrix i) m-min)
	    (setf m-min (aref matrix i)))
	  (when (> (aref matrix i) m-max)
	    (setf m-max (aref matrix i))))
	(values m-min m-max))
      (let ((m-min (aref matrix 0 0))
	    (m-max (aref matrix 0 0)))
	(dotimes (i (array-dimension matrix 0))
	  (dotimes (j (array-dimension matrix 1))
	    (when (< (aref matrix i j) m-min)
	      (setf m-min (aref matrix i j)))
	    (when (> (aref matrix i j) m-max)
	      (setf m-max (aref matrix i j)))))
	(values m-min m-max))))

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
