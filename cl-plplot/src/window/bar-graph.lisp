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
;;;; Functions relevant to the bar-graph class.
;;;;
;;;; hazen 8/06
;;;;

(in-package #:cl-plplot)

(defun number-of-dimensions (array)
  "Returns the number of dimesions in an array."
  (length (array-dimensions array)))

(defun bar-graph-check-dims (x data bar-widths colors)
  (labels ((check-len (arr arr-name)
	     (when (and arr
			(not (= (length arr) (array-dimension data 0))))
	       (format t "~A and data are not the same size! (~A != ~A)~%"
		       arr-name
		       (length arr)
		       (array-dimension data 0))
	       (return-from bar-graph-check-dims nil))))
    (check-len x "x")
    (check-len bar-widths "bar-widths")
    (check-len colors "colors"))
  t)

(defun new-bar-graph (x data &key bar-widths side-by-side colors (filled t) (copy t))
  "Creates a new bar-graph plot object.
   X is an array of size (n) specifying the centers of the bars with x[i] < x[i+1]. 
      If x is nil then data will be plotted against its index.
   Data should be an array of size (n x m), where m > 0.
   Bar-widths should be an array of size (n). It will specify the full width
      of each bar. Defaults are chosen if this is not specified.
   Side-by-side is t or nil. It specifies whether to draw the bars on
      top of each other or next to each other.
   Colors should be an array of symbols of size (m) specifying colors
      in the current color table.
   Filled specifies whether or not the bars are filled.
   If copy is true, then copies are made of x, data and widths, otherwise
      references are kept to the original vectors."
  (when (bar-graph-check-dims x data bar-widths colors)
    (make-instance 'bar-graph
		   :data-x (copy-vector (if x x (make-index-vector (array-dimension data 0))) copy)
		   :data (copy-vector data copy)
		   :bar-widths (copy-vector bar-widths copy)
		   :side-by-side side-by-side
		   :colors colors
		   :filled filled)))

(def-edit-method bar-graph (side-by-side colors filled)
  "edit-bar-graph, Edits the visual properties of a bar-graph.
    Set whether the bars are plotted side-by-side or on top of each other with side-by-side.
    Set the colors of the bars with colors.
    Set whether or not the bars are filled with filled.")

(defmethod plot-min-max ((a-bar-graph bar-graph))
  "Returns the minimum and maximum values of a bar graph as a 4 element vector."
  (let ((data (data-array a-bar-graph))
	(x-len (1- (length (data-x a-bar-graph))))
	(x-min)
	(x-max)
	(y-min 0)
	(y-max 0))
    (if (data-widths a-bar-graph)
	(progn
	  (setf x-min (- (aref (data-x a-bar-graph) 0) (* 0.6 (aref (bar-widths a-bar-graph) 0))))
	  (setf x-max (+ (aref (data-x a-bar-graph) x-len) (* 0.6 (aref (bar-widths a-bar-graph) x-len)))))
	(progn
	  (setf x-min (- (aref (data-x a-bar-graph) 0) 
			 (* 0.6 (- (aref (data-x a-bar-graph) 1) (aref (data-x a-bar-graph) 0)))))
	  (setf x-max (+ (aref (data-x a-bar-graph) x-len)
			 (* 0.6 (- (aref (data-x a-bar-graph) x-len) (aref (data-x a-bar-graph) (1- x-len))))))))
    (if (side-by-side a-bar-graph)
	(dotimes (i (array-dimension data 0))
	  (if (> (number-of-dimensions data) 1)
	      (dotimes (j (array-dimension data 1))
		(when (< (aref data i j) y-min) (setf y-min (aref data i j)))
		(when (> (aref data i j) y-max) (setf y-max (aref data i j))))
	      (progn
		(when (< (aref data i) y-min) (setf y-min (aref data i)))
		(when (> (aref data i) y-max) (setf y-max (aref data i))))))
	(dotimes (i (array-dimension data 0))
	  (let ((sum 0))
	    (if (> (number-of-dimensions data) 1)
		(dotimes (j (array-dimension data 1))
		  (incf sum (aref data i j)))
		(incf sum (aref data i)))
	    (when (< sum y-min) (setf y-min sum))
	    (when (> sum y-max) (setf y-max sum)))))
    (let ((diff (* 0.05 (- y-max y-min))))
      (cond
	((< (abs y-max) diff) 
	 (progn (decf y-min diff)
		(setf y-max 0.0)))
	((< (abs y-min) diff)
	 (progn (setf y-min 0.0)
		(incf y-max diff)))
	(t
	 (progn (decf y-min diff)
		(incf y-max diff)))))
    (values x-min x-max y-min y-max)))

(defmethod default-bar-widths ((a-bar-graph bar-graph))
  "Calculates default bar widths when these are not specified by the user."
  (let* ((data-x (data-x a-bar-graph))
	 (x-len (length data-x))
	 (default-widths (make-float-vector x-len))
	 (number-of-bars (if (side-by-side a-bar-graph)
			     (if (> (number-of-dimensions (data-array a-bar-graph)) 1)
				 (array-dimension (data-array a-bar-graph) 1)
				 1)
			     1)))
    (labels ((calc-width (x1 x2)
	       (* 0.9 (/ (- x1 x2) number-of-bars))))
      (dotimes (i (1- x-len))
	(setf (aref default-widths i) 
	      (calc-width (aref data-x (1+ i)) (aref data-x i)))
	(setf (aref default-widths (1- x-len))
	      (calc-width (aref data-x (1- x-len)) (aref data-x (- x-len 2))))))
    default-widths))
 
(defmethod render-plot ((a-bar-graph bar-graph) &optional ignored)
  "Renders a bar graph in the current window."
  (declare (ignore ignored))
  (let ((data-x (data-x a-bar-graph))
	(data (data-array a-bar-graph))
	(bar-widths (if (bar-widths a-bar-graph)
			(bar-widths a-bar-graph)
			(default-bar-widths a-bar-graph))))
    (if (= (number-of-dimensions (data-array a-bar-graph)) 1)
	(progn
	  (if (colors a-bar-graph)
	      (set-foreground-color (aref (colors a-bar-graph) 0))
	      (set-foregronnd-color nil))
	  (dotimes (i (length-data-x))
	    (let ((half-width (* 0.5 (aref bar-widths i))))
	      (

	(let ((
	(if (side-by-side a-bar-graph)
	    (let ((