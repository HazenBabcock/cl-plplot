;;;;
;;;; PLplot example 0
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(defun example0 (&optional (dev default-dev))
  (plsdev dev)
  (let* ((nsize 101)
	 (xmin 0.0)
	 (xmax 1.0)
	 (ymin 0.0)
	 (ymax 100.0)
	 (x (make-float-array nsize))
	 (y (make-float-array nsize)))
    (dotimes (i nsize)
      (setf (aref x i) (/ (float i) (float (1- nsize))))
      (setf (aref y i) (* ymax (aref x i) (aref x i))))

    (plinit)
    (plenv xmin xmax ymin ymax 0 0)
    (pllab "x" "y=100 x#u2#d" "Simple PLplot demo of a 2D line plot")
    (plline x y))
  (plend1))

;;;;
;;;; Copyright (c) 2014 Hazen P. Babcock
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
