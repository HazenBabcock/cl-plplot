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

; Am I using this?
;
;(defun find-last-type (a-list a-type)
;  "Findes the last object of a particular type in a list."
;  (find-if #'(lambda (x) (equal (type-of x) a-type)) a-list :from-end t))

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

(defun set-background-color (color)
  "Sets the background color."
  (set-color 0 color))

(defun set-color (which color)
  "Sets the current drawing color."
  (labels ((do-set-color (color-vec)
	     (plscol0 which (aref color-vec 0) (aref color-vec 1) (aref color-vec 2))
	     (plcol0 which)))
    (cond
      ((and color (symbolp color)) 
       (do-set-color (symbol-to-color color)))
      ((and color (vectorp color) (= 3 (length color))) 
       (do-set-color color))
      (t (set-color (if (= which 0)
			*background-color*
			*foreground-color*))))))

(defun set-foreground-color (color)
  "Sets the foreground color."
  (set-color 1 color))

(defun symbol-to-color (symbol)
  "Translates symbol into a color."
  (cond
    ((equal symbol :black) (vector 0 0 0))
    ((equal symbol :red) (vector 240 50 50))
    ((equal symbol :yellow) (vector 255 255 0))
    ((equal symbol :green) (vector 0 255 0))
    ((equal symbol :aquamarine) (vector 127 255 212))
    ((equal symbol :pink) (vector 255 192 203))
    ((equal symbol :wheat) (vector 245 222 179))
    ((equal symbol :grey) (vector 190 190 190))
    ((equal symbol :brown) (vector 165 42 42))
    ((equal symbol :blue) (vector 0 0 255))
    ((equal symbol :blue-violet) (vector 138 43 226))
    ((equal symbol :cyan) (vector 0 255 255))
    ((equal symbol :turquoise) (vector 64 224 208))
    ((equal symbol :magenta) (vector 255 0 255))
    ((equal symbol :salmon) (vector 250 128 114))
    ((equal symbol :white) (vector 255 255 255))
    (t (progn
	 (format t "Unrecognized color: ~A~%" symbol)
	 (vector 0 0 0)))))

