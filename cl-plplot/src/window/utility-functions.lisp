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

; should this throw an error?

(defun check-lengths (x y x-error y-error)
  "Checks that the arrays we have been given are appropriately defined."
  (let ((y-length (length y)))
    (when (and x (not (= y-length (length x))))
      (format t "y & x do not have the same number of elements!~%")
      (return-from check-lengths nil))
    (when (and x-error (not (= y-length (length x-error))))
      (format t "y & x-error do not have the same number of elements!~%")
      (return-from check-lengths nil))
    (when (and y-error (not (= y-length (length y-error))))
      (format t "y & y-error do not have the same number of elements!~%")
      (return-from check-lengths nil))
    t))

(defun error-bar (vec error)
  "Creates error bar vectors from vec to pass to plplot"
  (let* ((len (length vec))
	 (min-err (make-array len :element-type 'double-float))
	 (max-err (make-array len :element-type 'double-float)))
    (dotimes (i (length vec))
      (let ((err (* 0.5 (aref error i))))
	(setf (aref min-err i) (coerce (- (aref vec i) err) 'double-float))
	(setf (aref max-err i) (coerce (+ (aref vec i) err) 'double-float))))
    (values min-err max-err)))

(defun make-index-vector (vector-length)
  "Creates a vector of numbers 1,2,..,vector-length."
  (let ((index-vector (make-array vector-length :initial-element 0.0 :element-type 'float)))
    (dotimes (i vector-length)
      (setf (aref index-vector i) (1+ i)))
    index-vector))

; warning re. out of range value?
(defun range-check (val min max)
  "Returns true if val is between min and max (inclusive)."
  (and (>= val min)
       (<= val max)))


