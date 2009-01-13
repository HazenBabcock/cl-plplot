;;;
;;; Test clg-plot.
;;;

(defpackage :test
  (:use	:common-lisp
	:clg-plot))

(in-package :test)

(defun my-make-vector (dim init-fn)
  (let ((vec (make-array dim :initial-element 0.0 :element-type 'float)))
    (dotimes (i dim)
      (setf (aref vec i) (funcall init-fn i)))
    vec))

(defun new-plot-window (&optional name)
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p (cl-plplot:new-x-y-plot x y))
	 (w (cl-plplot:basic-window)))
    (cl-plplot:add-plot-to-window w p)
    (if name
	(new-clg-plot w :title name)
	(new-clg-plot w))))

(defun multi-window (n)
  (dotimes (i n)
    (new-plot-window (format nil "Plot ~A" i))))

;;;;
;;;; Copyright (c) 2008 Hazen P. Babcock
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
