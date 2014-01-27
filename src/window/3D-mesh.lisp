;;;;
;;;; Functions that are most closely related to the 3D-mesh class.
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

(defun new-3D-mesh (data-x data-y data-z &key contour-levels (copy t) (line-width 1) (line-style 1) 
		    (line-color *foreground-color*) (grid-type :grid-xy) contour-options curtain)
   "Creates a new 3D mesh (surface) plot.
    data-x specifies the x values of the points in data-z. If data-x is nil then data-z will 
       be plotted against its row index in x.
    data-y specifies the y avlues of the points in data-z. If data-y is nil then data-z will 
       be plotted against its column index in y.
    data-z is a 2D array of z values for the plot.
    contour-levels specifies the levels at which to draw contours, if desired. If this is
       not specified, default values are chosen.
    If copy is true then copies of data-x, data-y and data-z will be made, otherwise reference
       will be kept to the original vectors.
    line-width should be an integer line width, or zero if no line is desired
    line-style specifies what style line to draw (if a line is drawn), this should be a number
       between 1 and 8.
    line-color is the color to use for the lines in the plot.
    grid-type should be one of :gridx, :gridy or :gridxy. This specifies whether to draw
       lines only in x, only in y, or in both dimensions between the data points.
    contour-options should be one of nil, :magnitude-contour, :base-contour or :both.
       nil - no contours.
       :magnitude-contour - draw contour lines on the plot.
       :base-contour - draw contour lines on the x-y plane below the plot.
       :both - draw both magnitude and base contours.
    curtain should be t or nil. This specifies whether to draw a 'curtain' around the
       edges of the plot."
   (when (3D-plot-check-lengths data-x data-y data-z)
     (make-instance '3D-mesh
		    :data-x (copy-vector (if data-x data-x (make-index-vector (array-dimension data-z 0))) copy)
		    :data-y (copy-vector (if data-y data-y (make-index-vector (array-dimension data-z 1))) copy)
		    :data-z (copy-matrix data-z copy)
		    :contour-levels (check-contour-levels data-z (copy-vector contour-levels copy))
		    :line-width line-width
		    :line-style line-style
		    :line-color line-color
		    :grid-type grid-type
		    :contour-options contour-options
		    :curtain curtain)))

(def-edit-method 3D-mesh (line-width line-style line-color grid-type contour-options curtain)
  "edit-3D-mesh, Edits the visual properties of a 3D mesh
    Set the line width with :line-width (integer, 0 means no line).
    Set the line style with :line-style (integer between 1 and 8).
    Set the line color with :line-color symbol.
    Set the grid type with :grid-type to one of (:gridx, :gridy or :gridxy).
    Set the contour options with :contour-options to one of (:magnitude-contour,
       :base-contour or :both)
    Set the whether or not display a curtain with :curtain")

(defmethod plot-min-max ((a-plot 3D-plot))
  "Returns the minimum and maximum values in the 3D plot class object as a 6 element vector."
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
    (vector min-x max-x min-y max-y min-z max-z)))

;; draw a plot

(defmethod render-plot ((a-plot 3D-mesh) &optional (default-symbol 0))
  "Renders a 3D plot in the current window."
  (declare (ignore default-symbol))
  (set-foreground-color (line-color a-plot))
  (when (> (line-width a-plot) 0)
    (when (range-check (line-style a-plot) 1 8)
      (pllsty (line-style a-plot)))
    (plwidth (line-width a-plot)))
  (let ((parsed-options
	 (+ (cond
	      ((equal (grid-type a-plot) :grid-x) 1)
	      ((equal (grid-type a-plot) :grid-y) 2)
	      ((equal (grid-type a-plot) :grid-xy) 3)
	      (t
	       (format t "Unknown grid option : ~A, using default option~%" (grid-type a-plot))
	       3))
	    (cond
	      ((equal (contour-options a-plot) :magnitude-contour) 4)
	      ((equal (contour-options a-plot) :base-contour) 8)
	      ((equal (contour-options a-plot) :both) (+ 4 8))
	      (t
	       (when (contour-options a-plot)
		 (format t "Unknown contour option : ~A, using default option~%" (contour-options a-plot)))
	       0))
	    (if (curtain a-plot) 64 0))))
    (if (contour-options a-plot)
	(if (curtain a-plot)
	    (plot3dc (data-x a-plot) (data-y a-plot) (data-z a-plot) parsed-options (contour-levels a-plot))
	    (plmeshc (data-x a-plot) (data-y a-plot) (data-z a-plot) parsed-options (contour-levels a-plot)))
	(plmesh (data-x a-plot) (data-y a-plot) (data-z a-plot) parsed-options))))

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
