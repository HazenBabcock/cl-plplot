;;;;
;;;; Functions that are most closely related to the 3D-plot class.
;;;;
;;;; hazen 12/06
;;;;

(in-package #:cl-plplot)

(defun 3D-plot-check-lengths (data-x data-y data-z)
  "Checks that the arrays we have been given are of the appropriate size."
  (when (and data-x (/= (length data-x) (array-dimension data-z 0)))
    (format t "data-x and data-z do not have the right dimensions! (~A /= ~A)~%"
	    (length data-x) (array-dimension data-z 0))
    (return-from 3D-plot-check-lengths nil))
  (when (and data-y (/= (length data-y) (array-dimension data-z 1)))
    (format t "data-y and data-z do not have the right dimensions! (~A /= ~A)~%"
	    (length data-y) (array-dimension data-z 1))
    (return-from 3D-plot-check-lengths nil))
  t)

(defun new-3D-plot (data-x data-y data-z &key (copy t) (line-width 1) (line-style 1) (line-color *foreground-color*) surface)
   "Creates a new 3D (surface) plot.
    data-z is a 2D array of z values for the plot.
    data-x specifies the x values of the points in data-z. If data-x is nil then data-z will 
       be plotted against its row index in x.
    data-y specifies the y avlues of the points in data-z. If data-y is nil then data-z will 
       be plotted against its column index in y.
    If copy is true then copies of data-x, data-y and data-z will be made, otherwise reference
       will be kept to the original vectors.
    line-width should be an integer line width, or zero if no line is desired
    line-style specifies what style line to draw (if a line is drawn), this should be a number
       between 1 and 8.
    line-color is the color to use for the lines in the plot.
    surface is a symbol specifying what type of surface to. It should be one of:
       :mesh, :solid, :solid-mesh or :contour."
   (when (3D-plot-check-lengths data-x data-y data-z)
     (make-instance '3D-plot
		    :data-x (copy-vector (if data-x data-x (make-index-vector (array-dimension data-z 0))) copy)
		    :data-y (copy-vector (if data-y data-y (make-index-vector (array-dimension data-z 1))) copy)
		    :data-z (copy-vector data-z copy)
		    :line-width line-width
		    :line-style line-style
		    :line-color line-color
		    :surface surface)))

(def-edit-method 3D-plot (line-width line-style line-color surface)
  "edit-3D-plot, Edits the visual properties of a 3D plot
    Set the line width with :line-width (integer, 0 means no line).
    Set the line style with :line-style (integer between 1 and 8).
    Set the line color with :line-color symbol.
    Set the surface type with :surface to one of (:mesh, :solid, :solid-mesh or :contour)")

(defmethod plot-min-max ((a-plot 3D-plot))
  "Returns the minimum and maximum values in the 3D plot as a 6 element vector."
  (let* ((x-data (data-x a-plot))
	 (y-data (data-y a-plot))
	 (z-data (data-z a-plot))
	 (min-x (aref x-data 0))
	 (max-x (aref x-data 0))
	 (min-y (aref y-data 0))
	 (max-y (aref y-data 0))
	 (min-z (aref z-data 0 0))
	 (max-z (aref z-data 0 0)))
    (dotimes (i (length x-data))
      (when (< (aref x-data i) min-x) (setf min-x (aref x-data i)))
      (when (> (aref x-data i) max-x) (setf max-x (aref x-data i)))
      (when (< (aref y-data i) min-y) (setf min-y (aref y-data i)))
      (when (> (aref y-data i) max-y) (setf max-y (aref y-data i)))
      (dotimes (j (length y-data))
	(when (< (aref z-data i j) min-z) (setf min-z (aref z-data i j)))
	(when (> (aref z-data i j) max-z) (setf max-z (aref z-data i j)))))
    (let ((x-delta (* 0.05 (- max-x min-x)))
	  (y-delta (* 0.05 (- max-y min-y))))
      (vector (- min-x x-delta) (+ max-x x-delta) 
	      (- min-y y-delta) (+ max-y y-delta)
	      min-z max-z))))

;; draw a plot

(defmethod render-plot ((a-plot 3D-plot) &optional (default-symbol 0))
  "Renders a 3D plot in the current window."
  (declare (ignore default-symbol))
  (set-foreground-color (color a-plot))
  (when (> (line-width a-plot) 0)
    (when (range-check (line-style a-plot) 1 8)
      (pllsty (line-style a-plot)))
    (plwid (line-width a-plot)))
  (plmesh (data-x a-plot) (data-y a-plot) (data-z a-plot) 3))

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
